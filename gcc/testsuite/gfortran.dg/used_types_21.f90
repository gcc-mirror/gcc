! { dg-do compile }
! Check that pointer components are allowed to empty types.

TYPE :: empty_t
END TYPE empty_t

TYPE :: comp_t
  TYPE(empty_t), POINTER :: ptr
END TYPE comp_t

END
