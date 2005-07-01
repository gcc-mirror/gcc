------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        G N A T . P E R F E C T _ H A S H _ G E N E R A T O R S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002-2004 Ada Core Technologies, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Table;

package body GNAT.Perfect_Hash_Generators is

   --  We are using the algorithm of J. Czech as described in Zbigniew
   --  J. Czech, George Havas, and Bohdan S. Majewski ``An Optimal
   --  Algorithm for Generating Minimal Perfect Hash Functions'',
   --  Information Processing Letters, 43(1992) pp.257-264, Oct.1992

   --  This minimal perfect hash function generator is based on random
   --  graphs and produces a hash function of the form:

   --             h (w) = (g (f1 (w)) + g (f2 (w))) mod m

   --  where f1 and f2 are functions that map strings into integers,
   --  and g is a function that maps integers into [0, m-1]. h can be
   --  order preserving. For instance, let W = {w_0, ..., w_i, ...,
   --  w_m-1}, h can be defined such that h (w_i) = i.

   --  This algorithm defines two possible constructions of f1 and
   --  f2. Method b) stores the hash function in less memory space at
   --  the expense of greater CPU time.

   --  a) fk (w) = sum (for i in 1 .. length (w)) (Tk (i, w (i))) mod n

   --     size (Tk) = max (for w in W) (length (w)) * size (used char set)

   --  b) fk (w) = sum (for i in 1 .. length (w)) (Tk (i) * w (i)) mod n

   --     size (Tk) = max (for w in W) (length (w)) but the table
   --     lookups are replaced by multiplications.

   --  where Tk values are randomly generated. n is defined later on
   --  but the algorithm recommends to use a value a little bit
   --  greater than 2m. Note that for large values of m, the main
   --  memory space requirements comes from the memory space for
   --  storing function g (>= 2m entries).

   --  Random graphs are frequently used to solve difficult problems
   --  that do not have polynomial solutions. This algorithm is based
   --  on a weighted undirected graph. It comprises two steps: mapping
   --  and assigment.

   --  In the mapping step, a graph G = (V, E) is constructed, where V
   --  = {0, 1, ..., n-1} and E = {(for w in W) (f1 (w), f2 (w))}. In
   --  order for the assignment step to be successful, G has to be
   --  acyclic. To have a high probability of generating an acyclic
   --  graph, n >= 2m. If it is not acyclic, Tk have to be regenerated.

   --  In the assignment step, the algorithm builds function g. As G
   --  is acyclic, there is a vertex v1 with only one neighbor v2. Let
   --  w_i be the word such that v1 = f1 (w_i) and v2 = f2 (w_i). Let
   --  g (v1) = 0 by construction and g (v2) = (i - g (v1)) mod n (or
   --  to be general, (h (i) - g (v1) mod n). If word w_j is such that
   --  v2 = f1 (w_j) and v3 = f2 (w_j), g (v3) = (j - g (v2)) mod n
   --  (or to be general, (h (j) - g (v2)) mod n). If w_i has no
   --  neighbor, then another vertex is selected. The algorithm
   --  traverses G to assign values to all the vertices. It cannot
   --  assign a value to an already assigned vertex as G is acyclic.

   subtype Word_Id   is Integer;
   subtype Key_Id    is Integer;
   subtype Vertex_Id is Integer;
   subtype Edge_Id   is Integer;
   subtype Table_Id  is Integer;

   No_Vertex : constant Vertex_Id := -1;
   No_Edge   : constant Edge_Id   := -1;
   No_Table  : constant Table_Id  := -1;

   Max_Word_Length : constant := 32;
   subtype Word_Type is String (1 .. Max_Word_Length);
   Null_Word : constant Word_Type := (others => ASCII.NUL);
   --  Store keyword in a word. Note that the length of word is
   --  limited to 32 characters.

   type Key_Type is record
      Edge : Edge_Id;
   end record;
   --  A key corresponds to an edge in the algorithm graph.

   type Vertex_Type is record
      First : Edge_Id;
      Last  : Edge_Id;
   end record;
   --  A vertex can be involved in several edges. First and Last are
   --  the bounds of an array of edges stored in a global edge table.

   type Edge_Type is record
      X   : Vertex_Id;
      Y   : Vertex_Id;
      Key : Key_Id;
   end record;
   --  An edge is a peer of vertices. In the algorithm, a key
   --  is associated to an edge.

   package WT is new GNAT.Table (Word_Type, Word_Id, 0, 32, 32);
   package IT is new GNAT.Table (Integer, Integer, 0, 32, 32);
   --  The two main tables. IT is used to store several tables of
   --  components containing only integers.

   function Image (Int : Integer; W : Natural := 0) return String;
   function Image (Str : String;  W : Natural := 0) return String;
   --  Return a string which includes string Str or integer Int
   --  preceded by leading spaces if required by width W.

   Output : File_Descriptor renames GNAT.OS_Lib.Standout;
   --  Shortcuts

   Max  : constant := 78;
   Last : Natural  := 0;
   Line : String (1 .. Max);
   --  Use this line to provide buffered IO

   procedure Add (C : Character);
   procedure Add (S : String);
   --  Add a character or a string in Line and update Last

   procedure Put
     (F  : File_Descriptor;
      S  : String;
      F1 : Natural;
      L1 : Natural;
      C1 : Natural;
      F2 : Natural;
      L2 : Natural;
      C2 : Natural);
   --  Write string S into file F as a element of an array of one or
   --  two dimensions. Fk (resp. Lk and Ck) indicates the first (resp
   --  last and current) index in the k-th dimension. If F1 = L1 the
   --  array is considered as a one dimension array. This dimension is
   --  described by F2 and L2. This routine takes care of all the
   --  parenthesis, spaces and commas needed to format correctly the
   --  array. Moreover, the array is well indented and is wrapped to
   --  fit in a 80 col line. When the line is full, the routine writes
   --  it into file F. When the array is completed, the routine adds a
   --  semi-colon and writes the line into file F.

   procedure New_Line
     (F : File_Descriptor);
   --  Simulate Ada.Text_IO.New_Line with GNAT.OS_Lib

   procedure Put
     (F : File_Descriptor;
      S : String);
   --  Simulate Ada.Text_IO.Put with GNAT.OS_Lib

   procedure Put_Used_Char_Set
     (File  : File_Descriptor;
      Title : String);
   --  Output a title and a used character set

   procedure Put_Int_Vector
     (File   : File_Descriptor;
      Title  : String;
      Root   : Integer;
      Length : Natural);
   --  Output a title and a vector

   procedure Put_Int_Matrix
     (File  : File_Descriptor;
      Title : String;
      Table : Table_Id);
   --  Output a title and a matrix. When the matrix has only one
   --  non-empty dimension, it is output as a vector.

   procedure Put_Edges
     (File  : File_Descriptor;
      Title : String);
   --  Output a title and an edge table

   procedure Put_Initial_Keys
     (File  : File_Descriptor;
      Title : String);
   --  Output a title and a key table

   procedure Put_Reduced_Keys
     (File  : File_Descriptor;
      Title : String);
   --  Output a title and a key table

   procedure Put_Vertex_Table
     (File  : File_Descriptor;
      Title : String);
   --  Output a title and a vertex table

   ----------------------------------
   -- Character Position Selection --
   ----------------------------------

   --  We reduce the maximum key size by selecting representative
   --  positions in these keys. We build a matrix with one word per
   --  line. We fill the remaining space of a line with ASCII.NUL. The
   --  heuristic selects the position that induces the minimum number
   --  of collisions. If there are collisions, select another position
   --  on the reduced key set responsible of the collisions. Apply the
   --  heuristic until there is no more collision.

   procedure Apply_Position_Selection;
   --  Apply Position selection and build the reduced key table

   procedure Parse_Position_Selection (Argument : String);
   --  Parse Argument and compute the position set. Argument is a
   --  list of substrings separated by commas. Each substring
   --  represents a position or a range of positions (like x-y).

   procedure Select_Character_Set;
   --  Define an optimized used character set like Character'Pos in
   --  order not to allocate tables of 256 entries.

   procedure Select_Char_Position;
   --  Find a min char position set in order to reduce the max key
   --  length. The heuristic selects the position that induces the
   --  minimum number of collisions. If there are collisions, select
   --  another position on the reduced key set responsible of the
   --  collisions. Apply the heuristic until there is no collision.

   -----------------------------
   -- Random Graph Generation --
   -----------------------------

   procedure Random (Seed : in out Natural);
   --  Simulate Ada.Discrete_Numerics.Random.

   procedure Generate_Mapping_Table
     (T  : Table_Id;
      L1 : Natural;
      L2 : Natural;
      S  : in out Natural);
   --  Random generation of the tables below. T is already allocated.

   procedure Generate_Mapping_Tables
     (Opt : Optimization;
      S   : in out Natural);
   --  Generate the mapping tables T1 and T2. They are used to define :
   --  fk (w) = sum (for i in 1 .. length (w)) (Tk (i, w (i))) mod n.
   --  Keys, NK and Chars are used to compute the matrix size.

   ---------------------------
   -- Algorithm Computation --
   ---------------------------

   procedure Compute_Edges_And_Vertices (Opt : Optimization);
   --  Compute the edge and vertex tables. These are empty when a self
   --  loop is detected (f1 (w) = f2 (w)). The edge table is sorted by
   --  X value and then Y value. Keys is the key table and NK the
   --  number of keys. Chars is the set of characters really used in
   --  Keys. NV is the number of vertices recommended by the
   --  algorithm. T1 and T2 are the mapping tables needed to compute
   --  f1 (w) and f2 (w).

   function Acyclic return Boolean;
   --  Return True when the graph is acyclic. Vertices is the current
   --  vertex table and Edges the current edge table.

   procedure Assign_Values_To_Vertices;
   --  Execute the assignment step of the algorithm. Keys is the
   --  current key table. Vertices and Edges represent the random
   --  graph. G is the result of the assignment step such that:
   --    h (w) = (g (f1 (w)) + g (f2 (w))) mod m

   function Sum
     (Word  : Word_Type;
      Table : Table_Id;
      Opt   : Optimization)
      return  Natural;
   --  For an optimization of CPU_Time return
   --    fk (w) = sum (for i in 1 .. length (w)) (Tk (i, w (i))) mod n
   --  For an optimization of Memory_Space return
   --    fk (w) = sum (for i in 1 .. length (w)) (Tk (i) * w (i)) mod n
   --  Here NV = n

   -------------------------------
   -- Internal Table Management --
   -------------------------------

   function  Allocate (N : Natural; S : Natural) return Table_Id;
   --  procedure Deallocate (N : Natural; S : Natural);

   ----------
   -- Keys --
   ----------

   Key_Size : constant := 1;
   Keys     : Table_Id := No_Table;
   NK       : Natural;
   --  NK : Number of Keys

   function Initial (K : Key_Id) return Word_Id;
   pragma Inline (Initial);

   function Reduced (K : Key_Id) return Word_Id;
   pragma Inline (Reduced);

   function  Get_Key (F : Key_Id) return Key_Type;
   procedure Set_Key (F : Key_Id; Item : Key_Type);
   --  Comments needed here ???

   ------------------
   -- Char_Pos_Set --
   ------------------

   Char_Pos_Size    : constant := 1;
   Char_Pos_Set     : Table_Id := No_Table;
   Char_Pos_Set_Len : Natural;
   --  Character Selected Position Set

   function  Get_Char_Pos (P : Natural) return Natural;
   procedure Set_Char_Pos (P : Natural; Item : Natural);
   --  Comments needed here ???

   -------------------
   -- Used_Char_Set --
   -------------------

   Used_Char_Size    : constant := 1;
   Used_Char_Set     : Table_Id := No_Table;
   Used_Char_Set_Len : Natural;
   --  Used Character Set : Define a new character mapping. When all
   --  the characters are not present in the keys, in order to reduce
   --  the size of some tables, we redefine the character mapping.

   function  Get_Used_Char (C : Character) return Natural;
   procedure Set_Used_Char (C : Character; Item : Natural);

   -------------------
   -- Random Tables --
   -------------------

   Rand_Tab_Item_Size : constant := 1;
   T1                 : Table_Id := No_Table;
   T2                 : Table_Id := No_Table;
   Rand_Tab_Len_1     : Natural;
   Rand_Tab_Len_2     : Natural;
   --  T1  : Values table to compute F1
   --  T2  : Values table to compute F2

   function  Get_Rand_Tab (T : Integer; X, Y : Natural) return Natural;
   procedure Set_Rand_Tab (T : Integer; X, Y : Natural; Item : Natural);

   ------------------
   -- Random Graph --
   ------------------

   Graph_Item_Size : constant := 1;
   G               : Table_Id := No_Table;
   Graph_Len       : Natural;
   --  G   : Values table to compute G

   function  Get_Graph (F : Natural) return Integer;
   procedure Set_Graph (F : Natural; Item : Integer);
   --  Comments needed ???

   -----------
   -- Edges --
   -----------

   Edge_Size : constant := 3;
   Edges     : Table_Id := No_Table;
   Edges_Len : Natural;
   --  Edges  : Edge table of the random graph G

   function  Get_Edges (F : Natural) return Edge_Type;
   procedure Set_Edges (F : Natural; Item : Edge_Type);

   --------------
   -- Vertices --
   --------------

   Vertex_Size : constant := 2;

   Vertices : Table_Id := No_Table;
   --  Vertex table of the random graph G

   NV : Natural;
   --  Number of Vertices

   function  Get_Vertices (F : Natural) return Vertex_Type;
   procedure Set_Vertices (F : Natural; Item : Vertex_Type);
   --  Comments needed ???

   K2V : Float;
   --  Ratio between Keys and Vertices (parameter of Czech's algorithm)

   Opt : Optimization;
   --  Optimization mode (memory vs CPU)

   MKL : Natural;
   --  Maximum of all the word length

   S : Natural;
   --  Seed

   function Type_Size (L : Natural) return Natural;
   --  Given the last L of an unsigned integer type T, return its size

   -------------
   -- Acyclic --
   -------------

   function Acyclic return Boolean
   is
      Marks : array (0 .. NV - 1) of Vertex_Id := (others => No_Vertex);

      function Traverse
        (Edge  : Edge_Id;
         Mark  : Vertex_Id)
         return  Boolean;
      --  Propagate Mark from X to Y. X is already marked. Mark Y and
      --  propagate it to the edges of Y except the one representing
      --  the same key. Return False when Y is marked with Mark.

      --------------
      -- Traverse --
      --------------

      function Traverse
        (Edge  : Edge_Id;
         Mark  : Vertex_Id)
         return  Boolean
      is
         E : constant Edge_Type := Get_Edges (Edge);
         K : constant Key_Id    := E.Key;
         Y : constant Vertex_Id := E.Y;
         M : constant Vertex_Id := Marks (E.Y);
         V : Vertex_Type;

      begin
         if M = Mark then
            return False;

         elsif M = No_Vertex then
            Marks (Y) := Mark;
            V := Get_Vertices (Y);

            for J in V.First .. V.Last loop

               --  Do not propagate to the edge representing the same key.

               if Get_Edges (J).Key /= K
                 and then not Traverse (J, Mark)
               then
                  return False;
               end if;
            end loop;
         end if;

         return True;
      end Traverse;

      Edge  : Edge_Type;

   --  Start of processing for Acyclic

   begin
      --  Edges valid range is

      for J in 1 .. Edges_Len - 1 loop

         Edge := Get_Edges (J);

         --  Mark X of E when it has not been already done

         if Marks (Edge.X) = No_Vertex then
            Marks (Edge.X) := Edge.X;
         end if;

         --  Traverse E when this has not already been done

         if Marks (Edge.Y) = No_Vertex
           and then not Traverse (J, Edge.X)
         then
            return False;
         end if;
      end loop;

      return True;
   end Acyclic;

   ---------
   -- Add --
   ---------

   procedure Add (C : Character) is
   begin
      Line (Last + 1) := C;
      Last := Last + 1;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (S : String) is
      Len : constant Natural := S'Length;

   begin
      Line (Last + 1 .. Last + Len) := S;
      Last := Last + Len;
   end Add;

   --------------
   -- Allocate --
   --------------

   function  Allocate (N : Natural; S : Natural) return Table_Id is
      L : constant Integer := IT.Last;

   begin
      IT.Set_Last (L + N * S);
      return L + 1;
   end Allocate;

   ------------------------------
   -- Apply_Position_Selection --
   ------------------------------

   procedure Apply_Position_Selection is
   begin
      WT.Set_Last (2 * NK - 1);
      for J in 0 .. NK - 1 loop
         declare
            I_Word : constant Word_Type := WT.Table (Initial (J));
            R_Word : Word_Type := Null_Word;
            Index  : Natural   := I_Word'First - 1;

         begin
            --  Select the characters of Word included in the
            --  position selection.

            for C in 0 .. Char_Pos_Set_Len - 1 loop
               exit when I_Word (Get_Char_Pos (C)) = ASCII.NUL;
               Index := Index + 1;
               R_Word (Index) := I_Word (Get_Char_Pos (C));
            end loop;

            --  Build the new table with the reduced word

            WT.Table (Reduced (J)) := R_Word;
            Set_Key (J, (Edge => No_Edge));
         end;
      end loop;
   end Apply_Position_Selection;

   -------------
   -- Compute --
   -------------

   procedure Compute (Position : String := Default_Position) is
   begin
      Keys := Allocate (NK, Key_Size);

      if Verbose then
         Put_Initial_Keys (Output, "Initial Key Table");
      end if;

      if Position'Length /= 0 then
         Parse_Position_Selection (Position);
      else
         Select_Char_Position;
      end if;

      if Verbose then
         Put_Int_Vector
           (Output, "Char Position Set", Char_Pos_Set, Char_Pos_Set_Len);
      end if;

      Apply_Position_Selection;

      if Verbose then
         Put_Reduced_Keys (Output, "Reduced Keys Table");
      end if;

      Select_Character_Set;

      if Verbose then
         Put_Used_Char_Set (Output, "Character Position Table");
      end if;

      --  Perform Czech's algorithm

      loop
         Generate_Mapping_Tables (Opt, S);
         Compute_Edges_And_Vertices (Opt);

         --  When graph is not empty (no self-loop from previous
         --  operation) and not acyclic.

         exit when 0 < Edges_Len and then Acyclic;
      end loop;

      Assign_Values_To_Vertices;
   end Compute;

   -------------------------------
   -- Assign_Values_To_Vertices --
   -------------------------------

   procedure Assign_Values_To_Vertices is
      X  : Vertex_Id;

      procedure Assign (X : Vertex_Id);
      --  Execute assignment on X's neighbors except the vertex that
      --  we are coming from which is already assigned.

      ------------
      -- Assign --
      ------------

      procedure Assign (X : Vertex_Id)
      is
         E : Edge_Type;
         V : constant Vertex_Type := Get_Vertices (X);

      begin
         for J in V.First .. V.Last loop
            E := Get_Edges (J);
            if Get_Graph (E.Y) = -1 then
               Set_Graph (E.Y, (E.Key - Get_Graph (X)) mod NK);
               Assign (E.Y);
            end if;
         end loop;
      end Assign;

   --  Start of processing for Assign_Values_To_Vertices

   begin
      --  Value -1 denotes an unitialized value as it is supposed to
      --  be in the range 0 .. NK.

      if G = No_Table then
         Graph_Len := NV;
         G := Allocate (Graph_Len, Graph_Item_Size);
      end if;

      for J in 0 .. Graph_Len - 1 loop
         Set_Graph (J, -1);
      end loop;

      for K in 0 .. NK - 1 loop
         X := Get_Edges (Get_Key (K).Edge).X;

         if Get_Graph (X) = -1 then
            Set_Graph (X, 0);
            Assign (X);
         end if;
      end loop;

      for J in 0 .. Graph_Len - 1 loop
         if Get_Graph (J) = -1 then
            Set_Graph (J, 0);
         end if;
      end loop;

      if Verbose then
         Put_Int_Vector (Output, "Assign Values To Vertices", G, Graph_Len);
      end if;
   end Assign_Values_To_Vertices;

   --------------------------------
   -- Compute_Edges_And_Vertices --
   --------------------------------

   procedure Compute_Edges_And_Vertices (Opt : Optimization) is
      X           : Natural;
      Y           : Natural;
      Key         : Key_Type;
      Edge        : Edge_Type;
      Vertex      : Vertex_Type;
      Not_Acyclic : Boolean := False;

      procedure Move (From : Natural; To : Natural);
      function Lt (L, R : Natural) return Boolean;
      --  Subprograms needed for GNAT.Heap_Sort_A

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         Set_Edges (To, Get_Edges (From));
      end Move;

      --------
      -- Lt --
      --------

      function Lt (L, R : Natural) return Boolean is
         EL : constant Edge_Type := Get_Edges (L);
         ER : constant Edge_Type := Get_Edges (R);

      begin
         return EL.X < ER.X or else (EL.X = ER.X and then EL.Y < ER.Y);
      end Lt;

   --  Start of processing for Compute_Edges_And_Vertices

   begin
      --  We store edges from 1 to 2 * NK and leave
      --  zero alone in order to use GNAT.Heap_Sort_A.

      Edges_Len := 2 * NK + 1;

      if Edges = No_Table then
         Edges := Allocate (Edges_Len, Edge_Size);
      end if;

      if Vertices = No_Table then
         Vertices := Allocate (NV, Vertex_Size);
      end if;

      for J in 0 .. NV - 1 loop
         Set_Vertices (J, (No_Vertex, No_Vertex - 1));
      end loop;

      --  For each w, X = f1 (w) and Y = f2 (w)

      for J in 0 .. NK - 1 loop
         Key := Get_Key (J);
         Key.Edge := No_Edge;
         Set_Key (J, Key);

         X := Sum (WT.Table (Reduced (J)), T1, Opt);
         Y := Sum (WT.Table (Reduced (J)), T2, Opt);

         --  Discard T1 and T2 as soon as we discover a self loop

         if X = Y then
            Not_Acyclic := True;
            exit;
         end if;

         --  We store (X, Y) and (Y, X) to ease assignment step

         Set_Edges (2 * J + 1, (X, Y, J));
         Set_Edges (2 * J + 2, (Y, X, J));
      end loop;

      --  Return an empty graph when self loop detected

      if Not_Acyclic then
         Edges_Len := 0;

      else
         if Verbose then
            Put_Edges      (Output, "Unsorted Edge Table");
            Put_Int_Matrix (Output, "Function Table 1", T1);
            Put_Int_Matrix (Output, "Function Table 2", T2);
         end if;

         --  Enforce consistency between edges and keys. Construct
         --  Vertices and compute the list of neighbors of a vertex
         --  First .. Last as Edges is sorted by X and then Y. To
         --  compute the neighbor list, sort the edges.

         Sort
           (Edges_Len - 1,
            Move'Unrestricted_Access,
            Lt'Unrestricted_Access);

         if Verbose then
            Put_Edges      (Output, "Sorted Edge Table");
            Put_Int_Matrix (Output, "Function Table 1", T1);
            Put_Int_Matrix (Output, "Function Table 2", T2);
         end if;

         --  Edges valid range is 1 .. 2 * NK

         for E in 1 .. Edges_Len - 1 loop
            Edge := Get_Edges (E);
            Key  := Get_Key (Edge.Key);

            if Key.Edge = No_Edge then
               Key.Edge := E;
               Set_Key (Edge.Key, Key);
            end if;

            Vertex := Get_Vertices (Edge.X);

            if Vertex.First = No_Edge then
               Vertex.First := E;
            end if;

            Vertex.Last := E;
            Set_Vertices (Edge.X, Vertex);
         end loop;

         if Verbose then
            Put_Reduced_Keys (Output, "Key Table");
            Put_Edges        (Output, "Edge Table");
            Put_Vertex_Table (Output, "Vertex Table");
         end if;
      end if;
   end Compute_Edges_And_Vertices;

   ------------
   -- Define --
   ------------

   procedure Define
     (Name      : Table_Name;
      Item_Size : out Natural;
      Length_1  : out Natural;
      Length_2  : out Natural)
   is
   begin
      case Name is
         when Character_Position =>
            Item_Size := 8;
            Length_1  := Char_Pos_Set_Len;
            Length_2  := 0;

         when Used_Character_Set =>
            Item_Size := 8;
            Length_1  := 256;
            Length_2  := 0;

         when Function_Table_1
           |  Function_Table_2 =>
            Item_Size := Type_Size (NV);
            Length_1  := Rand_Tab_Len_1;
            Length_2  := Rand_Tab_Len_2;

         when Graph_Table =>
            Item_Size := Type_Size (NK);
            Length_1  := NV;
            Length_2  := 0;
      end case;
   end Define;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      WT.Release;
      IT.Release;

      Keys := No_Table;
      NK   := 0;

      Char_Pos_Set     := No_Table;
      Char_Pos_Set_Len := 0;

      Used_Char_Set     := No_Table;
      Used_Char_Set_Len := 0;

      T1 := No_Table;
      T2 := No_Table;

      Rand_Tab_Len_1 := 0;
      Rand_Tab_Len_2 := 0;

      G         := No_Table;
      Graph_Len := 0;

      Edges     := No_Table;
      Edges_Len := 0;

      Vertices     := No_Table;
      NV           := 0;
   end Finalize;

   ----------------------------
   -- Generate_Mapping_Table --
   ----------------------------

   procedure Generate_Mapping_Table
     (T  : Integer;
      L1 : Natural;
      L2 : Natural;
      S  : in out Natural)
   is
   begin
      for J in 0 .. L1 - 1 loop
         for K in 0 .. L2 - 1 loop
            Random (S);
            Set_Rand_Tab (T, J, K, S mod NV);
         end loop;
      end loop;
   end Generate_Mapping_Table;

   -----------------------------
   -- Generate_Mapping_Tables --
   -----------------------------

   procedure Generate_Mapping_Tables
     (Opt : Optimization;
      S   : in out Natural)
   is
   begin
      --  If T1 and T2 are already allocated no need to do it
      --  twice. Reuse them as their size has not changes.

      if T1 = No_Table and then T2 = No_Table then
         declare
            Used_Char_Last : Natural := 0;
            Used_Char      : Natural;

         begin
            if Opt = CPU_Time then
               for P in reverse Character'Range loop
                  Used_Char := Get_Used_Char (P);
                  if Used_Char /= 0 then
                     Used_Char_Last := Used_Char;
                     exit;
                  end if;
               end loop;
            end if;

            Rand_Tab_Len_1 := Char_Pos_Set_Len;
            Rand_Tab_Len_2 := Used_Char_Last + 1;
            T1 := Allocate (Rand_Tab_Len_1 * Rand_Tab_Len_2,
                            Rand_Tab_Item_Size);
            T2 := Allocate (Rand_Tab_Len_1 * Rand_Tab_Len_2,
                            Rand_Tab_Item_Size);
         end;
      end if;

      Generate_Mapping_Table (T1, Rand_Tab_Len_1, Rand_Tab_Len_2, S);
      Generate_Mapping_Table (T2, Rand_Tab_Len_1, Rand_Tab_Len_2, S);

      if Verbose then
         Put_Used_Char_Set (Output, "Used Character Set");
         Put_Int_Matrix (Output, "Function Table 1", T1);
         Put_Int_Matrix (Output, "Function Table 2", T2);
      end if;
   end Generate_Mapping_Tables;

   ------------------
   -- Get_Char_Pos --
   ------------------

   function Get_Char_Pos (P : Natural) return Natural is
      N : constant Natural := Char_Pos_Set + P;

   begin
      return IT.Table (N);
   end Get_Char_Pos;

   ---------------
   -- Get_Edges --
   ---------------

   function Get_Edges (F : Natural) return Edge_Type is
      N : constant Natural := Edges + (F * Edge_Size);
      E : Edge_Type;

   begin
      E.X   := IT.Table (N);
      E.Y   := IT.Table (N + 1);
      E.Key := IT.Table (N + 2);
      return E;
   end Get_Edges;

   ---------------
   -- Get_Graph --
   ---------------

   function Get_Graph (F : Natural) return Integer is
      N : constant Natural := G + F * Graph_Item_Size;

   begin
      return IT.Table (N);
   end Get_Graph;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (F : Key_Id) return Key_Type is
      N : constant Natural := Keys + F * Key_Size;
      K : Key_Type;

   begin
      K.Edge := IT.Table (N);
      return K;
   end Get_Key;

   ------------------
   -- Get_Rand_Tab --
   ------------------

   function Get_Rand_Tab (T : Integer; X, Y : Natural) return Natural is
      N : constant Natural :=
            T + ((Y * Rand_Tab_Len_1) + X) * Rand_Tab_Item_Size;

   begin
      return IT.Table (N);
   end Get_Rand_Tab;

   -------------------
   -- Get_Used_Char --
   -------------------

   function Get_Used_Char (C : Character) return Natural is
      N : constant Natural :=
            Used_Char_Set + Character'Pos (C) * Used_Char_Size;

   begin
      return IT.Table (N);
   end Get_Used_Char;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices (F : Natural) return Vertex_Type is
      N : constant Natural := Vertices + (F * Vertex_Size);
      V : Vertex_Type;

   begin
      V.First := IT.Table (N);
      V.Last  := IT.Table (N + 1);
      return V;
   end Get_Vertices;

   -----------
   -- Image --
   -----------

   function Image (Int : Integer; W : Natural := 0) return String is
      B : String (1 .. 32);
      L : Natural := 0;

      procedure Img (V : Natural);
      --  Compute image of V into B, starting at B (L), incrementing L

      ---------
      -- Img --
      ---------

      procedure Img (V : Natural) is
      begin
         if V > 9 then
            Img (V / 10);
         end if;

         L := L + 1;
         B (L) := Character'Val ((V mod 10) + Character'Pos ('0'));
      end Img;

   --  Start of processing for Image

   begin
      if Int < 0 then
         L := L + 1;
         B (L) := '-';
         Img (-Int);
      else
         Img (Int);
      end if;

      return Image (B (1 .. L), W);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Str : String; W : Natural := 0) return String is
      Len : constant Natural := Str'Length;
      Max : Natural := Len;

   begin
      if Max < W then
         Max := W;
      end if;

      declare
         Buf : String (1 .. Max) := (1 .. Max => ' ');

      begin
         for J in 0 .. Len - 1 loop
            Buf (Max - Len + 1 + J) := Str (Str'First + J);
         end loop;

         return Buf;
      end;
   end Image;

   -------------
   -- Initial --
   -------------

   function Initial (K : Key_Id) return Word_Id is
   begin
      return K;
   end Initial;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Seed   : Natural;
      K_To_V : Float        := Default_K_To_V;
      Optim  : Optimization := CPU_Time)
   is
   begin
      WT.Init;
      IT.Init;
      S   := Seed;

      Keys := No_Table;
      NK   := 0;

      Char_Pos_Set     := No_Table;
      Char_Pos_Set_Len := 0;

      K2V := K_To_V;
      Opt := Optim;
      MKL := 0;
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Value : String)
   is
      Word : Word_Type := Null_Word;
      Len  : constant Natural := Value'Length;

   begin
      Word (1 .. Len) := Value (Value'First .. Value'First + Len - 1);
      WT.Set_Last (NK);
      WT.Table (NK) := Word;
      NK := NK + 1;
      NV := Natural (Float (NK) * K2V);

      if MKL < Len then
         MKL := Len;
      end if;
   end Insert;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (F : File_Descriptor) is
      EOL : constant Character := ASCII.LF;

   begin
      if Write (F, EOL'Address, 1) /= 1 then
         raise Program_Error;
      end if;
   end New_Line;

   ------------------------------
   -- Parse_Position_Selection --
   ------------------------------

   procedure Parse_Position_Selection (Argument : String) is
      N : Natural          := Argument'First;
      L : constant Natural := Argument'Last;
      M : constant Natural := MKL;

      T : array (1 .. M) of Boolean := (others => False);

      function Parse_Index return Natural;
      --  Parse argument starting at index N to find an index

      -----------------
      -- Parse_Index --
      -----------------

      function Parse_Index return Natural
      is
         C : Character := Argument (N);
         V : Natural   := 0;

      begin
         if C = '$' then
            N := N + 1;
            return M;
         end if;

         if C not in '0' .. '9' then
            Raise_Exception
              (Program_Error'Identity, "cannot read position argument");
         end if;

         while C in '0' .. '9' loop
            V := V * 10 + (Character'Pos (C) - Character'Pos ('0'));
            N := N + 1;
            exit when L < N;
            C := Argument (N);
         end loop;

         return V;
      end Parse_Index;

   --  Start of processing for Parse_Position_Selection

   begin
      Char_Pos_Set_Len := 2 * NK;

      --  Empty specification means all the positions

      if L < N then
         Char_Pos_Set_Len := M;
         Char_Pos_Set := Allocate (Char_Pos_Set_Len, Char_Pos_Size);

         for C in 0 .. Char_Pos_Set_Len - 1 loop
            Set_Char_Pos (C, C + 1);
         end loop;

      else
         loop
            declare
               First, Last : Natural;

            begin
               First := Parse_Index;
               Last  := First;

               --  Detect a range

               if N <= L and then Argument (N) = '-' then
                  N := N + 1;
                  Last := Parse_Index;
               end if;

               --  Include the positions in the selection

               for J in First .. Last loop
                  T (J) := True;
               end loop;
            end;

            exit when L < N;

            if Argument (N) /= ',' then
               Raise_Exception
                 (Program_Error'Identity, "cannot read position argument");
            end if;

            N := N + 1;
         end loop;

         --  Compute position selection length

         N := 0;
         for J in T'Range loop
            if T (J) then
               N := N + 1;
            end if;
         end loop;

         --  Fill position selection

         Char_Pos_Set_Len := N;
         Char_Pos_Set := Allocate (Char_Pos_Set_Len, Char_Pos_Size);

         N := 0;
         for J in T'Range loop
            if T (J) then
               Set_Char_Pos (N, J);
               N := N + 1;
            end if;
         end loop;
      end if;
   end Parse_Position_Selection;

   -------------
   -- Produce --
   -------------

   procedure Produce (Pkg_Name  : String := Default_Pkg_Name) is
      File     : File_Descriptor;

      Status : Boolean;
      --  For call to Close;

      function Type_Img (L : Natural) return String;
      --  Return the larger unsigned type T such that T'Last < L

      function Range_Img (F, L : Natural; T : String := "") return String;
      --  Return string "[T range ]F .. L"

      function Array_Img (N, T, R1 : String; R2 : String := "") return String;
      --  Return string "N : constant array (R1[, R2]) of T;"

      --------------
      -- Type_Img --
      --------------

      function Type_Img (L : Natural) return String is
         S : constant String := Image (Type_Size (L));
         U : String  := "Unsigned_  ";
         N : Natural := 9;

      begin
         for J in S'Range loop
            N := N + 1;
            U (N) := S (J);
         end loop;

         return U (1 .. N);
      end Type_Img;

      ---------------
      -- Range_Img --
      ---------------

      function Range_Img (F, L : Natural; T : String := "") return String is
         FI  : constant String  := Image (F);
         FL  : constant Natural := FI'Length;
         LI  : constant String  := Image (L);
         LL  : constant Natural := LI'Length;
         TL  : constant Natural := T'Length;
         RI  : String (1 .. TL + 7 + FL + 4 + LL);
         Len : Natural := 0;

      begin
         if TL /= 0 then
            RI (Len + 1 .. Len + TL) := T;
            Len := Len + TL;
            RI (Len + 1 .. Len + 7) := " range ";
            Len := Len + 7;
         end if;

         RI (Len + 1 .. Len + FL) := FI;
         Len := Len + FL;
         RI (Len + 1 .. Len + 4) := " .. ";
         Len := Len + 4;
         RI (Len + 1 .. Len + LL) := LI;
         Len := Len + LL;
         return RI (1 .. Len);
      end Range_Img;

      ---------------
      -- Array_Img --
      ---------------

      function Array_Img
        (N, T, R1 : String;
         R2       : String := "")
         return     String
      is
      begin
         Last := 0;
         Add ("   ");
         Add (N);
         Add (" : constant array (");
         Add (R1);

         if R2 /= "" then
            Add (", ");
            Add (R2);
         end if;

         Add (") of ");
         Add (T);
         Add (" :=");
         return Line (1 .. Last);
      end Array_Img;

      F : Natural;
      L : Natural;
      P : Natural;

      PLen  : constant Natural := Pkg_Name'Length;
      FName : String (1 .. PLen + 4);

   --  Start of processing for Produce

   begin
      FName (1 .. PLen) := Pkg_Name;
      for J in 1 .. PLen loop
         if FName (J) in 'A' .. 'Z' then
            FName (J) := Character'Val (Character'Pos (FName (J))
                                        - Character'Pos ('A')
                                        + Character'Pos ('a'));

         elsif FName (J) = '.' then
            FName (J) := '-';
         end if;
      end loop;

      FName (PLen + 1 .. PLen + 4) := ".ads";

      File := Create_File (FName, Text);
      Put      (File, "package ");
      Put      (File, Pkg_Name);
      Put      (File, " is");
      New_Line (File);
      Put      (File, "   function Hash (S : String) return Natural;");
      New_Line (File);
      Put      (File, "end ");
      Put      (File, Pkg_Name);
      Put      (File, ";");
      New_Line (File);
      Close    (File, Status);

      if not Status then
         raise Device_Error;
      end if;

      FName (PLen + 4) := 'b';

      File := Create_File (FName, Text);
      Put      (File, "with Interfaces; use Interfaces;");
      New_Line (File);
      New_Line (File);
      Put      (File, "package body ");
      Put      (File, Pkg_Name);
      Put      (File, " is");
      New_Line (File);
      New_Line (File);

      if Opt = CPU_Time then
         Put      (File, Array_Img ("C", Type_Img (256), "Character"));
         New_Line (File);

         F := Character'Pos (Character'First);
         L := Character'Pos (Character'Last);

         for J in Character'Range loop
            P := Get_Used_Char (J);
            Put (File, Image (P), 0, 0, 0, F, L, Character'Pos (J));
         end loop;

         New_Line (File);
      end if;

      F := 0;
      L := Char_Pos_Set_Len - 1;

      Put      (File, Array_Img ("P", "Natural", Range_Img (F, L)));
      New_Line (File);

      for J in F .. L loop
         Put (File, Image (Get_Char_Pos (J)), 0, 0, 0, F, L, J);
      end loop;

      New_Line (File);

      if Opt = CPU_Time then
         Put_Int_Matrix
           (File,
            Array_Img ("T1", Type_Img (NV),
                       Range_Img (0, Rand_Tab_Len_1 - 1),
                       Range_Img (0, Rand_Tab_Len_2 - 1,
                                  Type_Img (256))),
            T1);

      else
         Put_Int_Matrix
           (File,
            Array_Img ("T1", Type_Img (NV),
                       Range_Img (0, Rand_Tab_Len_1 - 1)),
            T1);
      end if;

      New_Line (File);

      if Opt = CPU_Time then
         Put_Int_Matrix
           (File,
            Array_Img ("T2", Type_Img (NV),
                       Range_Img (0, Rand_Tab_Len_1 - 1),
                       Range_Img (0, Rand_Tab_Len_2 - 1,
                                  Type_Img (256))),
            T2);

      else
         Put_Int_Matrix
           (File,
            Array_Img ("T2", Type_Img (NV),
                       Range_Img (0, Rand_Tab_Len_1 - 1)),
            T2);
      end if;

      New_Line (File);

      Put_Int_Vector
        (File,
         Array_Img ("G", Type_Img (NK),
                    Range_Img (0, Graph_Len - 1)),
         G, Graph_Len);
      New_Line (File);

      Put      (File, "   function Hash (S : String) return Natural is");
      New_Line (File);
      Put      (File, "      F : constant Natural := S'First - 1;");
      New_Line (File);
      Put      (File, "      L : constant Natural := S'Length;");
      New_Line (File);
      Put      (File, "      F1, F2 : Natural := 0;");
      New_Line (File);

      Put (File, "      J : ");

      if Opt = CPU_Time then
         Put (File, Type_Img (256));
      else
         Put (File, "Natural");
      end if;

      Put (File, ";");
      New_Line (File);

      Put      (File, "   begin");
      New_Line (File);
      Put      (File, "      for K in P'Range loop");
      New_Line (File);
      Put      (File, "         exit when L < P (K);");
      New_Line (File);
      Put      (File, "         J  := ");

      if Opt = CPU_Time then
         Put (File, "C");
      else
         Put (File, "Character'Pos");
      end if;

      Put      (File, " (S (P (K) + F));");
      New_Line (File);

      Put (File, "         F1 := (F1 + Natural (T1 (K");

      if Opt = CPU_Time then
         Put (File, ", J");
      end if;

      Put (File, "))");

      if Opt = Memory_Space then
         Put (File, " * J");
      end if;

      Put      (File, ") mod ");
      Put      (File, Image (NV));
      Put      (File, ";");
      New_Line (File);

      Put (File, "         F2 := (F2 + Natural (T2 (K");

      if Opt = CPU_Time then
         Put (File, ", J");
      end if;

      Put (File, "))");

      if Opt = Memory_Space then
         Put (File, " * J");
      end if;

      Put      (File, ") mod ");
      Put      (File, Image (NV));
      Put      (File, ";");
      New_Line (File);

      Put      (File, "      end loop;");
      New_Line (File);

      Put      (File,
                "      return (Natural (G (F1)) + Natural (G (F2))) mod ");

      Put      (File, Image (NK));
      Put      (File, ";");
      New_Line (File);
      Put      (File, "   end Hash;");
      New_Line (File);
      New_Line (File);
      Put      (File, "end ");
      Put      (File, Pkg_Name);
      Put      (File, ";");
      New_Line (File);
      Close    (File, Status);

      if not Status then
         raise Device_Error;
      end if;
   end Produce;

   ---------
   -- Put --
   ---------

   procedure Put (F : File_Descriptor; S : String) is
      Len : constant Natural := S'Length;

   begin
      if Write (F, S'Address, Len) /= Len then
         raise Program_Error;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (F  : File_Descriptor;
      S  : String;
      F1 : Natural;
      L1 : Natural;
      C1 : Natural;
      F2 : Natural;
      L2 : Natural;
      C2 : Natural)
   is
      Len : constant Natural := S'Length;

      procedure Flush;

      -----------
      -- Flush --
      -----------

      procedure Flush is
      begin
         Put (F, Line (1 .. Last));
         New_Line (F);
         Last := 0;
      end Flush;

   --  Start of processing for Put

   begin
      if C1 = F1 and then C2 = F2 then
         Last := 0;
      end if;

      if Last + Len + 3 > Max then
         Flush;
      end if;

      if Last = 0 then
         Line (Last + 1 .. Last + 5) := "     ";
         Last := Last + 5;

         if F1 /= L1 then
            if C1 = F1 and then C2 = F2 then
               Add ('(');
            else
               Add (' ');
            end if;
         end if;
      end if;

      if C2 = F2 then
         Add ('(');
      else
         Add (' ');
      end if;

      Line (Last + 1 .. Last + Len) := S;
      Last := Last + Len;

      if C2 = L2 then
         Add (')');

         if F1 = L1 then
            Add (';');
            Flush;
         elsif C1 /= L1 then
            Add (',');
            Flush;
         else
            Add (')');
            Add (';');
            Flush;
         end if;

      else
         Add (',');
      end if;
   end Put;

   -----------------------
   -- Put_Used_Char_Set --
   -----------------------

   procedure Put_Used_Char_Set
     (File  : File_Descriptor;
      Title : String)
   is
      F : constant Natural := Character'Pos (Character'First);
      L : constant Natural := Character'Pos (Character'Last);

   begin
      Put (File, Title);
      New_Line (File);

      for J in Character'Range loop
         Put
           (File, Image (Get_Used_Char (J)), 0, 0, 0, F, L, Character'Pos (J));
      end loop;
   end Put_Used_Char_Set;

   ----------
   -- Put --
   ----------

   procedure Put_Int_Matrix
     (File   : File_Descriptor;
      Title  : String;
      Table  : Integer)
   is
      F1 : constant Natural := 0;
      L1 : constant Natural := Rand_Tab_Len_1 - 1;
      F2 : constant Natural := 0;
      L2 : constant Natural := Rand_Tab_Len_2 - 1;

   begin
      Put (File, Title);
      New_Line (File);

      if L2 = F2 then
         for J in F1 .. L1 loop
            Put (File,
                 Image (Get_Rand_Tab (Table, J, F2)), 0, 0, 0, F1, L1, J);
         end loop;

      else
         for J in F1 .. L1 loop
            for K in F2 .. L2 loop
               Put (File,
                    Image (Get_Rand_Tab (Table, J, K)), F1, L1, J, F2, L2, K);
            end loop;
         end loop;
      end if;
   end Put_Int_Matrix;

   --------------------
   -- Put_Int_Vector --
   --------------------

   procedure Put_Int_Vector
     (File   : File_Descriptor;
      Title  : String;
      Root   : Integer;
      Length : Natural)
   is
      F2 : constant Natural := 0;
      L2 : constant Natural := Length - 1;

   begin
      Put (File, Title);
      New_Line (File);

      for J in F2 .. L2 loop
         Put (File, Image (IT.Table (Root + J)), 0, 0, 0, F2, L2, J);
      end loop;
   end Put_Int_Vector;

   ---------------
   -- Put_Edges --
   ---------------

   procedure Put_Edges
     (File  : File_Descriptor;
      Title : String)
   is
      E  : Edge_Type;
      F1 : constant Natural := 1;
      L1 : constant Natural := Edges_Len - 1;
      M  : constant Natural := Max / 5;

   begin
      Put (File, Title);
      New_Line (File);

      --  Edges valid range is 1 .. Edge_Len - 1

      for J in F1 .. L1 loop
         E := Get_Edges (J);
         Put (File, Image (J, M),     F1, L1, J, 1, 4, 1);
         Put (File, Image (E.X, M),   F1, L1, J, 1, 4, 2);
         Put (File, Image (E.Y, M),   F1, L1, J, 1, 4, 3);
         Put (File, Image (E.Key, M), F1, L1, J, 1, 4, 4);
      end loop;
   end Put_Edges;

   ---------------------------
   -- Put_Initial_Keys --
   ---------------------------

   procedure Put_Initial_Keys
     (File  : File_Descriptor;
      Title : String)
   is
      F1 : constant Natural := 0;
      L1 : constant Natural := NK - 1;
      M  : constant Natural := Max / 5;
      K  : Key_Type;

   begin
      Put (File, Title);
      New_Line (File);

      for J in F1 .. L1 loop
         K := Get_Key (J);
         Put (File, Image (J, M),           F1, L1, J, 1, 3, 1);
         Put (File, Image (K.Edge, M),      F1, L1, J, 1, 3, 2);
         Put (File, WT.Table (Initial (J)), F1, L1, J, 1, 3, 3);
      end loop;
   end Put_Initial_Keys;

   ---------------------------
   -- Put_Reduced_Keys --
   ---------------------------

   procedure Put_Reduced_Keys
     (File  : File_Descriptor;
      Title : String)
   is
      F1 : constant Natural := 0;
      L1 : constant Natural := NK - 1;
      M  : constant Natural := Max / 5;
      K  : Key_Type;

   begin
      Put (File, Title);
      New_Line (File);

      for J in F1 .. L1 loop
         K := Get_Key (J);
         Put (File, Image (J, M),           F1, L1, J, 1, 3, 1);
         Put (File, Image (K.Edge, M),      F1, L1, J, 1, 3, 2);
         Put (File, WT.Table (Reduced (J)), F1, L1, J, 1, 3, 3);
      end loop;
   end Put_Reduced_Keys;

   ----------------------
   -- Put_Vertex_Table --
   ----------------------

   procedure Put_Vertex_Table
     (File  : File_Descriptor;
      Title : String)
   is
      F1 : constant Natural := 0;
      L1 : constant Natural := NV - 1;
      M  : constant Natural := Max / 4;
      V  : Vertex_Type;

   begin
      Put (File, Title);
      New_Line (File);

      for J in F1 .. L1 loop
         V := Get_Vertices (J);
         Put (File, Image (J, M),       F1, L1, J, 1, 3, 1);
         Put (File, Image (V.First, M), F1, L1, J, 1, 3, 2);
         Put (File, Image (V.Last, M),  F1, L1, J, 1, 3, 3);
      end loop;
   end Put_Vertex_Table;

   ------------
   -- Random --
   ------------

   procedure Random (Seed : in out Natural)
   is
      --  Park & Miller Standard Minimal using Schrage's algorithm to
      --  avoid overflow: Xn+1 = 16807 * Xn mod (2 ** 31 - 1)

      R : Natural;
      Q : Natural;
      X : Integer;

   begin
      R := Seed mod 127773;
      Q := Seed / 127773;
      X := 16807 * R - 2836 * Q;

      if X < 0 then
         Seed := X + 2147483647;
      else
         Seed := X;
      end if;
   end Random;

   -------------
   -- Reduced --
   -------------

   function Reduced (K : Key_Id) return Word_Id is
   begin
      return K + NK;
   end Reduced;

   --------------------------
   -- Select_Character_Set --
   --------------------------

   procedure Select_Character_Set
   is
      Last : Natural := 0;
      Used : array (Character) of Boolean := (others => False);

   begin
      for J in 0 .. NK - 1 loop
         for K in 1 .. Max_Word_Length loop
            exit when WT.Table (Initial (J))(K) = ASCII.NUL;
            Used (WT.Table (Initial (J))(K)) := True;
         end loop;
      end loop;

      Used_Char_Set_Len := 256;
      Used_Char_Set := Allocate (Used_Char_Set_Len, Used_Char_Size);

      for J in Used'Range loop
         if Used (J) then
            Set_Used_Char (J, Last);
            Last := Last + 1;
         else
            Set_Used_Char (J, 0);
         end if;
      end loop;
   end Select_Character_Set;

   --------------------------
   -- Select_Char_Position --
   --------------------------

   procedure Select_Char_Position is

      type Vertex_Table_Type is array (Natural range <>) of Vertex_Type;

      procedure Build_Identical_Keys_Sets
        (Table : in out Vertex_Table_Type;
         Last  : in out Natural;
         Pos   : in Natural);
      --  Build a list of keys subsets that are identical with the
      --  current position selection plus Pos. Once this routine is
      --  called, reduced words are sorted by subsets and each item
      --  (First, Last) in Sets defines the range of identical keys.

      function Count_Identical_Keys
        (Table  : Vertex_Table_Type;
         Last   : Natural;
         Pos    : Natural)
         return   Natural;
      --  For each subset in Sets, count the number of identical keys
      --  if we add Pos to the current position selection.

      Sel_Position : IT.Table_Type (1 .. MKL);
      Last_Sel_Pos : Natural := 0;

      -------------------------------
      -- Build_Identical_Keys_Sets --
      -------------------------------

      procedure Build_Identical_Keys_Sets
        (Table : in out Vertex_Table_Type;
         Last  : in out Natural;
         Pos   : in Natural)
      is
         S : constant Vertex_Table_Type := Table (1 .. Last);
         C : constant Natural           := Pos;
         --  Shortcuts

         F : Integer;
         L : Integer;
         --  First and last words of a subset

      begin
         Last := 0;

         --  For each subset in S, extract the new subsets we have by
         --  adding C in the position selection.

         for J in S'Range loop
            declare
               Offset : Natural;
               --  GNAT.Heap_Sort assumes that the first array index
               --  is 1. Offset defines the translation to operate.

               procedure Move (From : Natural; To : Natural);
               function Lt (L, R : Natural) return Boolean;
               --  Subprograms needed by GNAT.Heap_Sort_A

               ----------
               -- Move --
               ----------

               procedure Move (From : Natural; To : Natural) is
                  Target, Source : Natural;

               begin
                  if From = 0 then
                     Source := 0;
                     Target := Offset + To;
                  elsif To = 0 then
                     Source := Offset + From;
                     Target := 0;
                  else
                     Source := Offset + From;
                     Target := Offset + To;
                  end if;

                  WT.Table (Reduced (Target)) := WT.Table (Reduced (Source));
               end Move;

               --------
               -- Lt --
               --------

               function Lt (L, R : Natural) return Boolean is
                  C     : constant Natural := Pos;
                  Left  : Natural;
                  Right : Natural;

               begin
                  if L = 0 then
                     Left  := 0;
                     Right := Offset + R;
                  elsif R = 0 then
                     Left  := Offset + L;
                     Right := 0;
                  else
                     Left  := Offset + L;
                     Right := Offset + R;
                  end if;

                  return WT.Table (Reduced (Left))(C)
                       < WT.Table (Reduced (Right))(C);
               end Lt;

            --  Start of processing for Build_Identical_Key_Sets

            begin
               Offset := S (J).First - 1;
               Sort
                 (S (J).Last - S (J).First + 1,
                  Move'Unrestricted_Access,
                  Lt'Unrestricted_Access);

               F := -1;
               L := -1;
               for N in S (J).First .. S (J).Last - 1 loop

                  --  Two contiguous words are identical

                  if WT.Table (Reduced (N))(C) =
                     WT.Table (Reduced (N + 1))(C)
                  then
                     --  This is the first word of the subset

                     if F = -1 then
                        F := N;
                     end if;

                     L := N + 1;

                     --  This is the last word of the subset

                  elsif F /= -1 then
                     Last := Last + 1;
                     Table (Last) := (F, L);
                     F := -1;
                  end if;
               end loop;

               --  This is the last word of the subset and of the set

               if F /= -1 then
                  Last := Last + 1;
                  Table (Last) := (F, L);
               end if;
            end;
         end loop;
      end Build_Identical_Keys_Sets;

      --------------------------
      -- Count_Identical_Keys --
      --------------------------

      function Count_Identical_Keys
        (Table  : Vertex_Table_Type;
         Last   : Natural;
         Pos    : Natural)
         return   Natural
      is
         N : array (Character) of Natural;
         C : Character;
         T : Natural := 0;

      begin
         --  For each subset, count the number of words that are still
         --  identical when we include Sel_Position (Last_Sel_Pos) in
         --  the position selection. Only focus on this position as the
         --  other positions already produce identical keys.

         for S in 1 .. Last loop

            --  Count the occurrences of the different characters

            N := (others => 0);
            for K in Table (S).First .. Table (S).Last loop
               C := WT.Table (Reduced (K))(Pos);
               N (C) := N (C) + 1;
            end loop;

            --  Add to the total when there are two identical keys

            for J in N'Range loop
               if N (J) > 1 then
                  T := T + N (J);
               end if;
            end loop;
         end loop;

         return T;
      end Count_Identical_Keys;

   --  Start of processing for Select_Char_Position

   begin
      for C in Sel_Position'Range loop
         Sel_Position (C) := C;
      end loop;

      --  Initialization of Words

      WT.Set_Last (2 * NK - 1);

      for K in 0 .. NK - 1 loop
         WT.Table (Reduced (K) + 1) := WT.Table (Initial (K));
      end loop;

      declare
         Collisions           : Natural;
         Min_Collisions       : Natural := NK;
         Old_Collisions       : Natural;
         Min_Coll_Sel_Pos     : Natural := 0; -- init to kill warning
         Min_Coll_Sel_Pos_Idx : Natural := 0; -- init to kill warning
         Same_Keys_Sets_Table : Vertex_Table_Type (1 .. NK);
         Same_Keys_Sets_Last  : Natural := 1;

      begin
         Same_Keys_Sets_Table (1) := (1, NK);

         loop
            --  Preserve minimum identical keys and check later on
            --  that this value is strictly decrementing. Otherwise,
            --  it means that two keys are stricly identical.

            Old_Collisions := Min_Collisions;

            --  Find which position reduces the most of collisions

            for J in Last_Sel_Pos + 1 .. Sel_Position'Last loop
               Collisions := Count_Identical_Keys
                 (Same_Keys_Sets_Table,
                  Same_Keys_Sets_Last,
                  Sel_Position (J));

               if Collisions < Min_Collisions then
                  Min_Collisions       := Collisions;
                  Min_Coll_Sel_Pos     := Sel_Position (J);
                  Min_Coll_Sel_Pos_Idx := J;
               end if;
            end loop;

            if Old_Collisions = Min_Collisions then
               Raise_Exception
                 (Program_Error'Identity, "some keys are identical");
            end if;

            --  Insert selected position and sort Sel_Position table

            Last_Sel_Pos := Last_Sel_Pos + 1;
            Sel_Position (Last_Sel_Pos + 1 .. Min_Coll_Sel_Pos_Idx) :=
              Sel_Position (Last_Sel_Pos .. Min_Coll_Sel_Pos_Idx - 1);
            Sel_Position (Last_Sel_Pos) := Min_Coll_Sel_Pos;

            for P in 1 .. Last_Sel_Pos - 1 loop
               if Min_Coll_Sel_Pos < Sel_Position (P) then
                  Sel_Position (P + 1 .. Last_Sel_Pos) :=
                    Sel_Position (P .. Last_Sel_Pos - 1);
                  Sel_Position (P) := Min_Coll_Sel_Pos;
                  exit;
               end if;
            end loop;

            exit when Min_Collisions = 0;

            Build_Identical_Keys_Sets
              (Same_Keys_Sets_Table,
               Same_Keys_Sets_Last,
               Min_Coll_Sel_Pos);
         end loop;
      end;

      Char_Pos_Set_Len := Last_Sel_Pos;
      Char_Pos_Set := Allocate (Char_Pos_Set_Len, Char_Pos_Size);

      for C in 1 .. Last_Sel_Pos loop
         Set_Char_Pos (C - 1, Sel_Position (C));
      end loop;
   end Select_Char_Position;

   ------------------
   -- Set_Char_Pos --
   ------------------

   procedure Set_Char_Pos (P : Natural; Item : Natural) is
      N : constant Natural := Char_Pos_Set + P;

   begin
      IT.Table (N) := Item;
   end Set_Char_Pos;

   ---------------
   -- Set_Edges --
   ---------------

   procedure Set_Edges (F : Natural; Item : Edge_Type) is
      N : constant Natural := Edges + (F * Edge_Size);

   begin
      IT.Table (N)     := Item.X;
      IT.Table (N + 1) := Item.Y;
      IT.Table (N + 2) := Item.Key;
   end Set_Edges;

   ---------------
   -- Set_Graph --
   ---------------

   procedure Set_Graph (F : Natural; Item : Integer) is
      N : constant Natural := G + (F * Graph_Item_Size);

   begin
      IT.Table (N) := Item;
   end Set_Graph;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (F : Key_Id; Item : Key_Type) is
      N : constant Natural := Keys + F * Key_Size;

   begin
      IT.Table (N) := Item.Edge;
   end Set_Key;

   ------------------
   -- Set_Rand_Tab --
   ------------------

   procedure Set_Rand_Tab (T : Integer; X, Y : Natural; Item : Natural) is
      N : constant Natural :=
            T + ((Y * Rand_Tab_Len_1) + X) * Rand_Tab_Item_Size;

   begin
      IT.Table (N) := Item;
   end Set_Rand_Tab;

   -------------------
   -- Set_Used_Char --
   -------------------

   procedure Set_Used_Char (C : Character; Item : Natural) is
      N : constant Natural :=
            Used_Char_Set + Character'Pos (C) * Used_Char_Size;

   begin
      IT.Table (N) := Item;
   end Set_Used_Char;

   ------------------
   -- Set_Vertices --
   ------------------

   procedure Set_Vertices (F : Natural; Item : Vertex_Type) is
      N : constant Natural := Vertices + (F * Vertex_Size);

   begin
      IT.Table (N)     := Item.First;
      IT.Table (N + 1) := Item.Last;
   end Set_Vertices;

   ---------
   -- Sum --
   ---------

   function Sum
     (Word  : Word_Type;
      Table : Table_Id;
      Opt   : Optimization)
      return  Natural
   is
      S : Natural := 0;
      R : Natural;

   begin
      if Opt = CPU_Time then
         for J in 0 .. Rand_Tab_Len_1 - 1 loop
            exit when Word (J + 1) = ASCII.NUL;
            R := Get_Rand_Tab (Table, J, Get_Used_Char (Word (J + 1)));
            S := (S + R) mod NV;
         end loop;

      else
         for J in 0 .. Rand_Tab_Len_1 - 1 loop
            exit when Word (J + 1) = ASCII.NUL;
            R := Get_Rand_Tab (Table, J, 0);
            S := (S + R * Character'Pos (Word (J + 1))) mod NV;
         end loop;
      end if;

      return S;
   end Sum;

   ---------------
   -- Type_Size --
   ---------------

   function Type_Size (L : Natural) return Natural is
   begin
      if L <= 2 ** 8 then
         return 8;
      elsif L <= 2 ** 16 then
         return 16;
      else
         return 32;
      end if;
   end Type_Size;

   -----------
   -- Value --
   -----------

   function Value
     (Name : Table_Name;
      J   : Natural;
      K    : Natural := 0)
      return Natural
   is
   begin
      case Name is
         when Character_Position =>
            return Get_Char_Pos (J);

         when Used_Character_Set =>
            return Get_Used_Char (Character'Val (J));

         when Function_Table_1 =>
            return Get_Rand_Tab (T1, J, K);

         when  Function_Table_2 =>
            return Get_Rand_Tab (T2, J, K);

         when Graph_Table =>
            return Get_Graph (J);

      end case;
   end Value;

end GNAT.Perfect_Hash_Generators;
