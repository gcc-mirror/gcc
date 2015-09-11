with Ada.Containers.Vectors;

generic
   type Vertex_Key is private;
package Debug4_Pkg is

   type Vertex_Id is new Natural;
   subtype Valid_Vertex_Id is Vertex_Id range 1 .. Vertex_Id'Last;

   package VIL is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Valid_Vertex_Id);
   use VIL;
   subtype Vertex_Index_List is VIL.Vector;

   package VL is new Ada.Containers.Vectors
     (Index_Type   => Valid_Vertex_Id,
      Element_Type => Vertex_Key);
   use VL;
   subtype Vertex_List is VL.Vector;

   type T is tagged record
      Vertices : Vertex_List;
   end record;

   function Dominator_Tree (G : T'Class) return T;

end Debug4_Pkg;
