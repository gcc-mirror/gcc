package dispatch2_p is
  type Object     is tagged null record;
  type Object_Ptr is access all Object'CLASS;
--
  function Impl_Of (Self : access Object) return Object_Ptr;
  function Get_Ptr (Self : access Object) return Object_Ptr
    renames Impl_Of;
end;
