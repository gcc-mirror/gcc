! { dg-do run }
!
! Tests pointer function selectors in ASSOCIATE.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de> in
! https://gcc.gnu.org/pipermail/fortran/2024-March/060294.html
program paul
  implicit none
  type t
     integer :: i
  end type t
  type(t), pointer :: p(:)
  integer :: j
  allocate (p(-3:3))
  p% i = [(j,j=-3,3)]

  associate (q => p)
    print *, lbound (q), ubound (q) ! Should print -3 3 (OK)
    print *, q% i
  end associate

  associate (q => set_ptr())
    print *, lbound (q), ubound (q) ! Should print -3 3 (OK)
    print *, q(:)% i                ! <<< ... has no IMPLICIT type
  end associate

  associate (q => (p))
    print *, lbound (q), ubound (q) ! Should print 1 7 (OK)
    print *, q% i
  end associate

  associate (q => (set_ptr()))
    print *, lbound (q), ubound (q) ! Should print 1 7 (OK)
    print *, q(:)% i                ! <<< ... has no IMPLICIT type
  end associate
contains
  function set_ptr () result (res)
    type(t), pointer :: res(:)
    res => p
  end function set_ptr
end
