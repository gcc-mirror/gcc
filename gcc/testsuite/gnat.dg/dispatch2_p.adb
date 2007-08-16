--
package body dispatch2_p is
  function Impl_Of (Self : access Object) return Object_Ptr is
  begin
    return Object_Ptr (Self);
  end Impl_Of;
end;
