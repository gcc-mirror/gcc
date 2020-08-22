! { dg-do compile }
! PR fortran/95611 - ICE in access_attr_decl, at fortran/decl.c:9075

module m
  public operator (.a.)
  public operator (.a.) ! { dg-error "has already been specified" }
end
