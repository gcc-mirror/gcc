! { dg-do compile }
subroutine s
  type t
  1 ! { dg-error "Statement label without statement" }
  end type
end subroutine
