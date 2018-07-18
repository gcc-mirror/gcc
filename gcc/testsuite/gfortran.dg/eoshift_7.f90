! { dg-do compile }
program main
  type t
    integer :: x
  end type t
  type(t), dimension(2) :: a, b
  a(1)%x = 1
  a(2)%x = 2
  b = eoshift(a,1) ! { dg-error "Missing 'boundary' argument to 'eoshift' intrinsic" }
  print *,b%x
end program main
