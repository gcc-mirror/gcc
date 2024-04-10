! { dg-do run }
!
! PR fortran/114024

program foo
  implicit none
  complex :: cmp(3) = (3.,4.)
  type ci           ! pseudo "complex integer" type
     integer :: re
     integer :: im
  end type ci
  type cr           ! pseudo "complex" type
     real :: re
     real :: im
  end type cr
  type u
     type(ci) :: ii(3)
     type(cr) :: rr(3)
  end type u
  type(u) :: cc

  cc% ii% re = nint (cmp% re)
  cc% ii% im = nint (cmp% im)
  cc% rr% re = cmp% re
  cc% rr% im = cmp% im
 
  call test_substring ()
  call test_int_real ()
  call test_poly ()

contains

  subroutine test_substring ()
    character(4)              :: str(3) = ["abcd","efgh","ijkl"]
    character(:), allocatable :: ac(:)
    allocate (ac, source=str(1::2)(2:4))
    if (size (ac) /= 2 .or. len (ac) /= 3) stop 11
    if (ac(2) /= "jkl")                    stop 12
    deallocate (ac)
    allocate (ac, mold=str(1::2)(2:4))
    if (size (ac) /= 2 .or. len (ac) /= 3) stop 13
    deallocate (ac)
  end

  subroutine test_int_real ()
    integer, allocatable  :: aa(:)
    real, pointer         :: pp(:)
    allocate (aa, source = cc% ii% im)
    if (size (aa) /= 3)      stop 21
    if (any (aa /= cmp% im)) stop 22
    allocate (pp, source = cc% rr% re)
    if (size (pp) /= 3)      stop 23
    if (any (pp /= cmp% re)) stop 24
    deallocate (aa, pp)
  end

  subroutine test_poly ()
    class(*), allocatable :: uu(:), vv(:)
    allocate (uu, source = cc% ii% im)
    allocate (vv, source = cc% rr% re)
    if (size (uu) /= 3) stop 31
    if (size (vv) /= 3) stop 32
    call check (uu)
    call check (vv)
    deallocate (uu, vv)
    allocate (uu, mold = cc% ii% im)
    allocate (vv, mold = cc% rr% re)
    if (size (uu) /= 3) stop 33
    if (size (vv) /= 3) stop 34
    deallocate (uu, vv)
  end

  subroutine check (x)
    class(*), intent(in) :: x(:)
    select type (x)
    type is (integer)
       if (any (x /= cmp% im)) then
          print *, "'integer':", x
          stop 41
       end if
    type is (real)
       if (any (x /= cmp% re)) then
          print *, "'real':", x
          stop 42
       end if
    type is (character(*))
       print *, "'character':", x
    end select
  end
end
