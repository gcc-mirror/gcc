package body Debug4_Pkg is

   type Vertex_To_Vertex_T is array (Vertex_Id range <>) of Vertex_Id;

   function Dominator_Tree_Internal (G : T'Class) return Vertex_To_Vertex_T is
      subtype V_To_V is Vertex_To_Vertex_T (0 .. G.Vertices.Last_Index);
      type V_To_VIL is array
        (Valid_Vertex_Id range 1 .. G.Vertices.Last_Index)
        of Vertex_Index_List;
      Bucket : V_To_VIL := (others => VIL.Empty_Vector);
      Dom    : V_To_V   := (others => 0);
   begin
      return Dom;
   end;

   function Dominator_Tree (G : T'Class) return T is
      Dom : constant Vertex_To_Vertex_T := Dominator_Tree_Internal (G);
      DT  : T := (Vertices => VL.Empty_Vector);
   begin
      return DT;
   end;

end Debug4_Pkg;
