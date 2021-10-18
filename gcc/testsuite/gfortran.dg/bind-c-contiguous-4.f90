! { dg-do run }
! 
! Same test as bind-c-contiguous-1.* but with OPTIONAL 
!
! { dg-additional-sources bind-c-contiguous-4.c }
! { dg-additional-options "-fcheck=all" }
! { dg-prune-output "command-line option '-fcheck=.*' is valid for Fortran but not for C" }

! Fortran demands that with bind(C), the callee ensure that for
! * 'contiguous'
! * len=* with explicit/assumed-size arrays
! noncontiguous actual arguments are handled.
! (in without bind(C) in gfortran, caller handles the copy in/out

! Additionally, for a bind(C) callee, a Fortran-written caller
! has to ensure the same (for contiguous + len=* to explicit-/assumed-size arrays)

module m
  use iso_c_binding, only: c_intptr_t, c_bool, c_loc, c_int
  implicit none (type, external)

  type, bind(C) :: loc_t
    integer(c_intptr_t) :: x, y, z
  end type loc_t

interface
  type(loc_t) function char_assumed_size_c (xx, yy, zz, n, num) bind(C)
    import :: loc_t, c_bool, c_int
    integer(c_int), value :: n, num
    character(len=*), optional :: xx(*), yy(n:*), zz(6:6, 3:n, 3:*)
  end function

  type(loc_t) function char_assumed_size_in_c (xx, yy, zz, n, num) bind(C)
    import :: loc_t, c_bool, c_int
    integer(c_int), value :: n, num
    character(len=*), intent(in), optional :: xx(*), yy(n:*), zz(6:6, 3:n, 3:*)
  end function

  type(loc_t) function char_expl_size_c (xx, yy, zz, n, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer(c_int), value :: n, num
    character(len=*), optional :: xx(n), yy(n:n+3), zz(6:6, 3:n, 3:n+3)
  end function

  type(loc_t) function char_expl_size_in_c (xx, yy, zz, n, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer(c_int), value :: n, num
    character(len=*), intent(in), optional :: xx(n), yy(n:n+3), zz(6:6, 3:n, 3:n+3)
  end function

  type(loc_t) function char_assumed_rank_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), optional :: xx(..)
    character(len=3), optional :: yy(..)
    character(len=k), optional :: zz(..)
  end function

  type(loc_t) function char_assumed_rank_in_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), intent(in), optional :: xx(..)
    character(len=3), intent(in), optional :: yy(..)
    character(len=k), intent(in), optional :: zz(..)
  end function

  type(loc_t) function char_assumed_rank_cont_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), contiguous, optional :: xx(..)
    character(len=3), contiguous, optional :: yy(..)
    character(len=k), contiguous, optional :: zz(..)
  end function

  type(loc_t) function char_assumed_rank_cont_in_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), contiguous, intent(in), optional :: xx(..)
    character(len=3), contiguous, intent(in), optional :: yy(..)
    character(len=k), contiguous, intent(in), optional :: zz(..)
  end function

  type(loc_t) function char_assumed_shape_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), optional :: xx(:)
    character(len=3), optional :: yy(5:)
    character(len=k), optional :: zz(-k:)
  end function

  type(loc_t) function char_assumed_shape_in_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), intent(in), optional :: xx(:)
    character(len=3), intent(in), optional :: yy(5:)
    character(len=k), intent(in), optional :: zz(-k:)
  end function

  type(loc_t) function char_assumed_shape_cont_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), contiguous, optional :: xx(:)
    character(len=3), contiguous, optional :: yy(5:)
    character(len=k), contiguous, optional :: zz(-k:)
  end function

  type(loc_t) function char_assumed_shape_cont_in_c (xx, yy, zz, k, num) bind(c)
    import :: loc_t, c_bool, c_int
    integer, value :: k, num
    character(len=*), contiguous, intent(in), optional :: xx(:)
    character(len=3), contiguous, intent(in), optional :: yy(5:)
    character(len=k), contiguous, intent(in), optional :: zz(-k:)
  end function
end interface

contains

type(loc_t) function char_assumed_size_f (xx, yy, zz, n, num) bind(c) result(res)
  integer, value :: num, n
  character(len=*), optional :: xx(*), yy(n:*), zz(6:6, 3:n, 3:*)
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= len(xx)) error stop 1
  if (3 /= len(yy)) error stop 1
  if (3 /= len(zz)) error stop 1
  if (1 /= lbound(xx,dim=1)) error stop 1
  if (3 /= lbound(yy,dim=1)) error stop 1
  if (6 /= lbound(zz,dim=1)) error stop 1
  if (3 /= lbound(zz,dim=2)) error stop 1
  if (3 /= lbound(zz,dim=3)) error stop 1
  if (1 /= size(zz,dim=1)) error stop 1
  if (1 /= size(zz,dim=2)) error stop 1
  if (6 /= ubound(zz,dim=1)) error stop 1
  if (3 /= ubound(zz,dim=2)) error stop 1
  if (num == 1) then
    if (xx(1) /= "abc") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "nop") error stop 4
    if (yy(3) /= "abc") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "nop") error stop 4
    if (zz(6,n,3) /= "abc") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "nop") error stop 4
  else if (num == 2) then
    if (xx(1) /= "def") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "jlm") error stop 4
    if (yy(3) /= "def") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "jlm") error stop 4
    if (zz(6,n,3) /= "def") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "jlm") error stop 4
  else
    error stop 8
  endif
  xx(1) = "ABC"
  xx(2) = "DEF"
  xx(3) = "GHI"
  yy(3) = "ABC"
  yy(4) = "DEF"
  yy(5) = "GHI"
  zz(6,n,3) = "ABC"
  zz(6,n,4) = "DEF"
  zz(6,n,5) = "GHI"
  res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
end

type(loc_t) function char_assumed_size_in_f (xx, yy, zz, n, num) bind(c) result(res)
  integer, value :: num, n
  character(len=*), optional :: xx(*), yy(n:*), zz(6:6, 3:n, 3:*)
  intent(in) :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= len(xx)) error stop 1
  if (3 /= len(yy)) error stop 1
  if (3 /= len(zz)) error stop 1
  if (1 /= lbound(xx,dim=1)) error stop 1
  if (3 /= lbound(yy,dim=1)) error stop 1
  if (6 /= lbound(zz,dim=1)) error stop 1
  if (3 /= lbound(zz,dim=2)) error stop 1
  if (3 /= lbound(zz,dim=3)) error stop 1
  if (1 /= size(zz,dim=1)) error stop 1
  if (1 /= size(zz,dim=2)) error stop 1
  if (6 /= ubound(zz,dim=1)) error stop 1
  if (3 /= ubound(zz,dim=2)) error stop 1
  if (num == 1) then
    if (xx(1) /= "abc") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "nop") error stop 4
    if (yy(3) /= "abc") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "nop") error stop 4
    if (zz(6,n,3) /= "abc") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "nop") error stop 4
  else if (num == 2) then
    if (xx(1) /= "def") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "jlm") error stop 4
    if (yy(3) /= "def") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "jlm") error stop 4
    if (zz(6,n,3) /= "def") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "jlm") error stop 4
  else
    error stop 8
  endif
  res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }  if (num == 1) then
end

type(loc_t) function char_expl_size_f (xx, yy, zz, n, num) bind(c) result(res)
  integer, value :: num, n
  character(len=*), optional :: xx(n), yy(n:n+2), zz(6:6, 3:n, 3:n+2)
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= len(xx)) error stop 1
  if (3 /= len(yy)) error stop 1
  if (3 /= len(zz)) error stop 1
  if (1 /= lbound(xx,dim=1)) error stop 1
  if (3 /= lbound(yy,dim=1)) error stop 1
  if (6 /= lbound(zz,dim=1)) error stop 1
  if (3 /= lbound(zz,dim=2)) error stop 1
  if (3 /= lbound(zz,dim=3)) error stop 1
  if (3 /= size(xx,dim=1)) error stop 1
  if (3 /= size(yy,dim=1)) error stop 1
  if (1 /= size(zz,dim=1)) error stop 1
  if (1 /= size(zz,dim=2)) error stop 1
  if (3 /= size(zz,dim=3)) error stop 1
  if (3 /= ubound(xx,dim=1)) error stop 1
  if (5 /= ubound(yy,dim=1)) error stop 1
  if (6 /= ubound(zz,dim=1)) error stop 1
  if (3 /= ubound(zz,dim=2)) error stop 1
  if (5 /= ubound(zz,dim=3)) error stop 1
  if (num == 1) then
    if (xx(1) /= "abc") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "nop") error stop 4
    if (yy(3) /= "abc") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "nop") error stop 4
    if (zz(6,n,3) /= "abc") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "nop") error stop 4
  else if (num == 2) then
    if (xx(1) /= "def") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "jlm") error stop 4
    if (yy(3) /= "def") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "jlm") error stop 4
    if (zz(6,n,3) /= "def") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "jlm") error stop 4
  else
    error stop 8
  endif
  xx(1) = "ABC"
  xx(2) = "DEF"
  xx(3) = "GHI"
  yy(3) = "ABC"
  yy(4) = "DEF"
  yy(5) = "GHI"
  zz(6,n,3) = "ABC"
  zz(6,n,4) = "DEF"
  zz(6,n,5) = "GHI"
  res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
end

type(loc_t) function char_expl_size_in_f (xx, yy, zz, n, num) bind(c) result(res)
  integer, value :: num, n
  character(len=*), optional :: xx(n), yy(n:n+2), zz(6:6, 3:n, 3:n+2)
  intent(in) :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= len(xx)) error stop 1
  if (3 /= len(yy)) error stop 1
  if (3 /= len(zz)) error stop 1
  if (1 /= lbound(xx,dim=1)) error stop 1
  if (3 /= lbound(yy,dim=1)) error stop 1
  if (6 /= lbound(zz,dim=1)) error stop 1
  if (3 /= lbound(zz,dim=2)) error stop 1
  if (3 /= lbound(zz,dim=3)) error stop 1
  if (3 /= size(xx,dim=1)) error stop 1
  if (3 /= size(yy,dim=1)) error stop 1
  if (1 /= size(zz,dim=1)) error stop 1
  if (1 /= size(zz,dim=2)) error stop 1
  if (3 /= size(zz,dim=3)) error stop 1
  if (3 /= ubound(xx,dim=1)) error stop 1
  if (5 /= ubound(yy,dim=1)) error stop 1
  if (6 /= ubound(zz,dim=1)) error stop 1
  if (3 /= ubound(zz,dim=2)) error stop 1
  if (5 /= ubound(zz,dim=3)) error stop 1
  if (num == 1) then
    if (xx(1) /= "abc") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "nop") error stop 4
    if (yy(3) /= "abc") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "nop") error stop 4
    if (zz(6,n,3) /= "abc") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "nop") error stop 4
  else if (num == 2) then
    if (xx(1) /= "def") error stop 2
    if (xx(2) /= "ghi") error stop 3
    if (xx(3) /= "jlm") error stop 4
    if (yy(3) /= "def") error stop 2
    if (yy(4) /= "ghi") error stop 3
    if (yy(5) /= "jlm") error stop 4
    if (zz(6,n,3) /= "def") error stop 2
    if (zz(6,n,4) /= "ghi") error stop 3
    if (zz(6,n,5) /= "jlm") error stop 4
  else
    error stop 8
  endif
  res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
end


type(loc_t) function char_assumed_rank_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(..)
  character(len=3), optional :: yy(..)
  character(len=k), optional :: zz(..)
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  if (3 /= len(xx)) error stop 40
  if (3 /= len(yy)) error stop 40
  if (3 /= len(zz)) error stop 40
  if (3 /= size(xx)) error stop 41
  if (3 /= size(yy)) error stop 41
  if (3 /= size(zz)) error stop 41
  if (1 /= rank(xx)) error stop 49
  if (1 /= rank(yy)) error stop 49
  if (1 /= rank(zz)) error stop 49
  if (1 /= lbound(xx, dim=1)) stop 49
  if (1 /= lbound(yy, dim=1)) stop 49
  if (1 /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (3 /= ubound(yy, dim=1)) stop 49
  if (3 /= ubound(zz, dim=1)) stop 49
  if (num == 1) then
    if (is_contiguous (xx)) error stop 49
    if (is_contiguous (yy)) error stop 49
    if (is_contiguous (zz)) error stop 49
  else if (num == 2) then
    if (.not. is_contiguous (xx)) error stop 49
    if (.not. is_contiguous (yy)) error stop 49
    if (.not. is_contiguous (zz)) error stop 49
  else
    error stop 48
  end if
  select rank (xx)
  rank (1)
    print *, xx(1:3)
    if (num == 1) then
      if (xx(1) /= "abc") error stop 42
      if (xx(2) /= "ghi") error stop 43
      if (xx(3) /= "nop") error stop 44
    else if (num == 2) then
      if (xx(1) /= "def") error stop 45
      if (xx(2) /= "ghi") error stop 46
      if (xx(3) /= "jlm") error stop 47
    else
      error stop 48
    endif
    xx(1) = "ABC"
    xx(2) = "DEF"
    xx(3) = "GHI"
    res%x = get_loc (xx)
  rank default
    error stop 99
  end select
  select rank (yy)
  rank (1)
    print *, yy(1:3)
    if (num == 1) then
      if (yy(1) /= "abc") error stop 42
      if (yy(2) /= "ghi") error stop 43
      if (yy(3) /= "nop") error stop 44
    else if (num == 2) then
      if (yy(1) /= "def") error stop 45
      if (yy(2) /= "ghi") error stop 46
      if (yy(3) /= "jlm") error stop 47
    else
      error stop 48
    endif
    yy(1) = "ABC"
    yy(2) = "DEF"
    yy(3) = "GHI"
    res%y = get_loc (yy)
  rank default
    error stop 99
  end select
  select rank (zz)
  rank (1)
    print *, zz(1:3)
    if (num == 1) then
      if (zz(1) /= "abc") error stop 42
      if (zz(2) /= "ghi") error stop 43
      if (zz(3) /= "nop") error stop 44
    else if (num == 2) then
      if (zz(1) /= "def") error stop 45
      if (zz(2) /= "ghi") error stop 46
      if (zz(3) /= "jlm") error stop 47
    else
      error stop 48
    endif
    zz(1) = "ABC"
    zz(2) = "DEF"
    zz(3) = "GHI"
    res%z = get_loc (zz)
  rank default
    error stop 99
  end select
contains
  integer (c_intptr_t) function get_loc (arg)
    character(len=*), target :: arg(:)
    ! %loc does copy in/out if not simply contiguous
    ! extra func needed because of 'target' attribute
    get_loc = transfer (c_loc(arg), res%x)
  end
end

type(loc_t) function char_assumed_rank_in_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(..)
  character(len=3), optional :: yy(..)
  character(len=k), optional :: zz(..)
  intent(in) :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  if (3 /= size(yy)) error stop 50
  if (3 /= len(yy)) error stop 51
  if (1 /= rank(yy)) error stop 59
  if (1 /= lbound(xx, dim=1)) stop 49
  if (1 /= lbound(yy, dim=1)) stop 49
  if (1 /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (3 /= ubound(yy, dim=1)) stop 49
  if (3 /= ubound(zz, dim=1)) stop 49
  if (num == 1) then
    if (is_contiguous (xx)) error stop 59
    if (is_contiguous (yy)) error stop 59
    if (is_contiguous (zz)) error stop 59
  else if (num == 2) then
    if (.not. is_contiguous (xx)) error stop 59
    if (.not. is_contiguous (yy)) error stop 59
    if (.not. is_contiguous (zz)) error stop 59
  else
    error stop 48
  end if
  select rank (xx)
  rank (1)
    print *, xx(1:3)
    if (num == 1) then
      if (xx(1) /= "abc") error stop 52
      if (xx(2) /= "ghi") error stop 53
      if (xx(3) /= "nop") error stop 54
    else if (num == 2) then
      if (xx(1) /= "def") error stop 55
      if (xx(2) /= "ghi") error stop 56
      if (xx(3) /= "jlm") error stop 57
    else
      error stop 58
    endif
    res%x = get_loc(xx)
  rank default
    error stop 99
  end select
  select rank (yy)
  rank (1)
    print *, yy(1:3)
    if (num == 1) then
      if (yy(1) /= "abc") error stop 52
      if (yy(2) /= "ghi") error stop 53
      if (yy(3) /= "nop") error stop 54
    else if (num == 2) then
      if (yy(1) /= "def") error stop 55
      if (yy(2) /= "ghi") error stop 56
      if (yy(3) /= "jlm") error stop 57
    else
      error stop 58
    endif
    res%y = get_loc(yy)
  rank default
    error stop 99
  end select
  select rank (zz)
  rank (1)
    print *, zz(1:3)
    if (num == 1) then
      if (zz(1) /= "abc") error stop 52
      if (zz(2) /= "ghi") error stop 53
      if (zz(3) /= "nop") error stop 54
    else if (num == 2) then
      if (zz(1) /= "def") error stop 55
      if (zz(2) /= "ghi") error stop 56
      if (zz(3) /= "jlm") error stop 57
    else
      error stop 58
    endif
    res%z = get_loc(zz)
  rank default
    error stop 99
  end select
contains
  integer (c_intptr_t) function get_loc (arg)
    character(len=*), target :: arg(:)
    ! %loc does copy in/out if not simply contiguous
    ! extra func needed because of 'target' attribute
    get_loc = transfer (c_loc(arg), res%x)
  end
end



type(loc_t) function char_assumed_rank_cont_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(..)
  character(len=3), optional :: yy(..)
  character(len=k), optional :: zz(..)
  contiguous :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  if (3 /= len(xx)) error stop 60
  if (3 /= len(yy)) error stop 60
  if (3 /= len(zz)) error stop 60
  if (3 /= size(xx)) error stop 61
  if (3 /= size(yy)) error stop 61
  if (3 /= size(zz)) error stop 61
  if (1 /= rank(xx)) error stop 69
  if (1 /= rank(yy)) error stop 69
  if (1 /= rank(zz)) error stop 69
  if (1 /= lbound(xx, dim=1)) stop 49
  if (1 /= lbound(yy, dim=1)) stop 49
  if (1 /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (3 /= ubound(yy, dim=1)) stop 49
  if (3 /= ubound(zz, dim=1)) stop 49
  select rank (xx)
  rank (1)
    print *, xx(1:3)
    if (num == 1) then
      if (xx(1) /= "abc") error stop 62
      if (xx(2) /= "ghi") error stop 63
      if (xx(3) /= "nop") error stop 64
    else if (num == 2) then
      if (xx(1) /= "def") error stop 65
      if (xx(2) /= "ghi") error stop 66
      if (xx(3) /= "jlm") error stop 67
    else
      error stop 68
    endif
    xx(1) = "ABC"
    xx(2) = "DEF"
    xx(3) = "GHI"
    res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
  select rank (yy)
  rank (1)
    print *, yy(1:3)
    if (num == 1) then
      if (yy(1) /= "abc") error stop 62
      if (yy(2) /= "ghi") error stop 63
      if (yy(3) /= "nop") error stop 64
    else if (num == 2) then
      if (yy(1) /= "def") error stop 65
      if (yy(2) /= "ghi") error stop 66
      if (yy(3) /= "jlm") error stop 67
    else
      error stop 68
    endif
    yy(1) = "ABC"
    yy(2) = "DEF"
    yy(3) = "GHI"
    res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
  select rank (zz)
  rank (1)
    print *, zz(1:3)
    if (num == 1) then
      if (zz(1) /= "abc") error stop 62
      if (zz(2) /= "ghi") error stop 63
      if (zz(3) /= "nop") error stop 64
    else if (num == 2) then
      if (zz(1) /= "def") error stop 65
      if (zz(2) /= "ghi") error stop 66
      if (zz(3) /= "jlm") error stop 67
    else
      error stop 68
    endif
    zz(1) = "ABC"
    zz(2) = "DEF"
    zz(3) = "GHI"
    res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
end

type(loc_t) function char_assumed_rank_cont_in_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(..)
  character(len=3), optional :: yy(..)
  character(len=k), optional :: zz(..)
  intent(in) :: xx, yy, zz
  contiguous :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  if (3 /= size(xx)) error stop 30
  if (3 /= size(yy)) error stop 30
  if (3 /= size(zz)) error stop 30
  if (3 /= len(xx)) error stop 31
  if (3 /= len(yy)) error stop 31
  if (3 /= len(zz)) error stop 31
  if (1 /= rank(xx)) error stop 69
  if (1 /= rank(yy)) error stop 69
  if (1 /= rank(zz)) error stop 69
  if (1 /= lbound(xx, dim=1)) stop 49
  if (1 /= lbound(yy, dim=1)) stop 49
  if (1 /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (3 /= ubound(yy, dim=1)) stop 49
  if (3 /= ubound(zz, dim=1)) stop 49
  select rank (xx)
  rank (1)
    print *, xx(1:3)
    if (num == 1) then
      if (xx(1) /= "abc") error stop 62
      if (xx(2) /= "ghi") error stop 63
      if (xx(3) /= "nop") error stop 64
    else if (num == 2) then
      if (xx(1) /= "def") error stop 65
      if (xx(2) /= "ghi") error stop 66
      if (xx(3) /= "jlm") error stop 67
    else
      error stop 68
    endif
    res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
  select rank (yy)
  rank (1)
    print *, yy(1:3)
    if (num == 1) then
      if (yy(1) /= "abc") error stop 62
      if (yy(2) /= "ghi") error stop 63
      if (yy(3) /= "nop") error stop 64
    else if (num == 2) then
      if (yy(1) /= "def") error stop 65
      if (yy(2) /= "ghi") error stop 66
      if (yy(3) /= "jlm") error stop 67
    else
      error stop 68
    endif
    res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
  select rank (zz)
  rank (1)
    print *, zz(1:3)
    if (num == 1) then
      if (zz(1) /= "abc") error stop 62
      if (zz(2) /= "ghi") error stop 63
      if (zz(3) /= "nop") error stop 64
    else if (num == 2) then
      if (zz(1) /= "def") error stop 65
      if (zz(2) /= "ghi") error stop 66
      if (zz(3) /= "jlm") error stop 67
    else
      error stop 68
    endif
    res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
end

type(loc_t) function char_assumed_shape_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(:)
  character(len=3), optional :: yy(5:)
  character(len=k), optional :: zz(-k:)
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= len(xx)) error stop 70
  if (3 /= len(yy)) error stop 70
  if (3 /= len(zz)) error stop 70
  if (3 /= size(xx)) error stop 71
  if (3 /= size(yy)) error stop 71
  if (3 /= size(zz)) error stop 71
  if (1 /= lbound(xx, dim=1)) stop 49
  if (5 /= lbound(yy, dim=1)) stop 49
  if (-k /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (7 /= ubound(yy, dim=1)) stop 49
  if (-k+2 /= ubound(zz, dim=1)) stop 49
  if (num == 1) then
    if (is_contiguous (xx)) error stop 79
    if (is_contiguous (yy)) error stop 79
    if (is_contiguous (zz)) error stop 79
    if (xx(1) /= "abc") error stop 72
    if (xx(2) /= "ghi") error stop 73
    if (xx(3) /= "nop") error stop 74
    if (yy(5) /= "abc") error stop 72
    if (yy(6) /= "ghi") error stop 73
    if (yy(7) /= "nop") error stop 74
    if (zz(-k) /= "abc") error stop 72
    if (zz(-k+1) /= "ghi") error stop 73
    if (zz(-k+2) /= "nop") error stop 74
  else if (num == 2) then
    if (.not.is_contiguous (xx)) error stop 79
    if (.not.is_contiguous (yy)) error stop 79
    if (.not.is_contiguous (zz)) error stop 79
    if (xx(1) /= "def") error stop 72
    if (xx(2) /= "ghi") error stop 73
    if (xx(3) /= "jlm") error stop 74
    if (yy(5) /= "def") error stop 72
    if (yy(6) /= "ghi") error stop 73
    if (yy(7) /= "jlm") error stop 74
    if (zz(-k) /= "def") error stop 72
    if (zz(-k+1) /= "ghi") error stop 73
    if (zz(-k+2) /= "jlm") error stop 74
  else
    error stop 78
  endif
  xx(1) = "ABC"
  xx(2) = "DEF"
  xx(3) = "GHI"
  yy(5) = "ABC"
  yy(6) = "DEF"
  yy(7) = "GHI"
  zz(-k) = "ABC"
  zz(-k+1) = "DEF"
  zz(-k+2) = "GHI"
  res%x = get_loc(xx)
  res%y = get_loc(yy)
  res%z = get_loc(zz)
contains
  integer (c_intptr_t) function get_loc (arg)
    character(len=*), target :: arg(:)
    ! %loc does copy in/out if not simply contiguous
    ! extra func needed because of 'target' attribute
    get_loc = transfer (c_loc(arg), res%x)
  end
end

type(loc_t) function char_assumed_shape_in_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(:)
  character(len=3), optional :: yy(5:)
  character(len=k), optional :: zz(-k:)
  intent(in) :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= size(xx)) error stop 80
  if (3 /= size(yy)) error stop 80
  if (3 /= size(zz)) error stop 80
  if (3 /= len(xx)) error stop 81
  if (3 /= len(yy)) error stop 81
  if (3 /= len(zz)) error stop 81
  if (1 /= lbound(xx, dim=1)) stop 49
  if (5 /= lbound(yy, dim=1)) stop 49
  if (-k /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (7 /= ubound(yy, dim=1)) stop 49
  if (-k+2 /= ubound(zz, dim=1)) stop 49
  if (num == 1) then
    if (is_contiguous (xx)) error stop 89
    if (is_contiguous (yy)) error stop 89
    if (is_contiguous (zz)) error stop 89
    if (xx(1) /= "abc") error stop 82
    if (xx(2) /= "ghi") error stop 83
    if (xx(3) /= "nop") error stop 84
    if (yy(5) /= "abc") error stop 82
    if (yy(6) /= "ghi") error stop 83
    if (yy(7) /= "nop") error stop 84
    if (zz(-k) /= "abc") error stop 82
    if (zz(-k+1) /= "ghi") error stop 83
    if (zz(-k+2) /= "nop") error stop 84
  else if (num == 2) then
    if (.not.is_contiguous (xx)) error stop 89
    if (.not.is_contiguous (yy)) error stop 89
    if (.not.is_contiguous (zz)) error stop 89
    if (xx(1) /= "def") error stop 85
    if (xx(2) /= "ghi") error stop 86
    if (xx(3) /= "jlm") error stop 87
    if (yy(5) /= "def") error stop 85
    if (yy(6) /= "ghi") error stop 86
    if (yy(7) /= "jlm") error stop 87
    if (zz(-k) /= "def") error stop 85
    if (zz(-k+1) /= "ghi") error stop 86
    if (zz(-k+2) /= "jlm") error stop 87
  else
    error stop 88
  endif
  res%x = get_loc(xx)
  res%y = get_loc(yy)
  res%z = get_loc(zz)
contains
  integer (c_intptr_t) function get_loc (arg)
    character(len=*), target :: arg(:)
    ! %loc does copy in/out if not simply contiguous
    ! extra func needed because of 'target' attribute
    get_loc = transfer (c_loc(arg), res%x)
  end
end



type(loc_t) function char_assumed_shape_cont_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(:)
  character(len=3), optional :: yy(5:)
  character(len=k), optional :: zz(-k:)
  contiguous :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= len(xx)) error stop 90
  if (3 /= len(yy)) error stop 90
  if (3 /= len(zz)) error stop 90
  if (3 /= size(xx)) error stop 91
  if (3 /= size(yy)) error stop 91
  if (3 /= size(zz)) error stop 91
  if (1 /= lbound(xx, dim=1)) stop 49
  if (5 /= lbound(yy, dim=1)) stop 49
  if (-k /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (7 /= ubound(yy, dim=1)) stop 49
  if (-k+2 /= ubound(zz, dim=1)) stop 49
  if (num == 1) then
    if (xx(1) /= "abc") error stop 92
    if (xx(2) /= "ghi") error stop 93
    if (xx(3) /= "nop") error stop 94
    if (yy(5) /= "abc") error stop 92
    if (yy(6) /= "ghi") error stop 93
    if (yy(7) /= "nop") error stop 94
    if (zz(-k) /= "abc") error stop 92
    if (zz(-k+1) /= "ghi") error stop 93
    if (zz(-k+2) /= "nop") error stop 94
  else if (num == 2) then
    if (xx(1) /= "def") error stop 92
    if (xx(2) /= "ghi") error stop 93
    if (xx(3) /= "jlm") error stop 94
    if (yy(5) /= "def") error stop 92
    if (yy(6) /= "ghi") error stop 93
    if (yy(7) /= "jlm") error stop 94
    if (zz(-k) /= "def") error stop 92
    if (zz(-k+1) /= "ghi") error stop 93
    if (zz(-k+2) /= "jlm") error stop 94
  else
    error stop 98
  endif
  xx(1) = "ABC"
  xx(2) = "DEF"
  xx(3) = "GHI"
  yy(5) = "ABC"
  yy(6) = "DEF"
  yy(7) = "GHI"
  zz(-k) = "ABC"
  zz(-k+1) = "DEF"
  zz(-k+2) = "GHI"
  res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
end

type(loc_t) function char_assumed_shape_cont_in_f (xx, yy, zz, k, num) bind(c) result(res)
  integer, value :: num, k
  character(len=*), optional :: xx(:)
  character(len=3), optional :: yy(5:)
  character(len=k), optional :: zz(-k:)
  intent(in) :: xx, yy, zz
  contiguous :: xx, yy, zz
  if (num == 3) then
    if (present (xx) .or. present (yy) .or. present (zz)) error stop 1
    res%x = -1; res%y = -1; res%z = -1
    return
  end if
  if (.not.present (xx) .or. .not.present (yy) .or. .not.present (zz)) error stop 1
  print *, xx(1:3)
  if (3 /= size(xx)) error stop 100
  if (3 /= size(yy)) error stop 100
  if (3 /= size(zz)) error stop 100
  if (3 /= len(xx)) error stop 101
  if (3 /= len(yy)) error stop 101
  if (3 /= len(zz)) error stop 101
  if (1 /= lbound(xx, dim=1)) stop 49
  if (5 /= lbound(yy, dim=1)) stop 49
  if (-k /= lbound(zz, dim=1)) stop 49
  if (3 /= ubound(xx, dim=1)) stop 49
  if (7 /= ubound(yy, dim=1)) stop 49
  if (-k+2 /= ubound(zz, dim=1)) stop 49
  if (num == 1) then
    if (xx(1) /= "abc") error stop 102
    if (xx(2) /= "ghi") error stop 103
    if (xx(3) /= "nop") error stop 104
    if (yy(5) /= "abc") error stop 102
    if (yy(6) /= "ghi") error stop 103
    if (yy(7) /= "nop") error stop 104
    if (zz(-k) /= "abc") error stop 102
    if (zz(-k+1) /= "ghi") error stop 103
    if (zz(-k+2) /= "nop") error stop 104
  else if (num == 2) then
    if (xx(1) /= "def") error stop 105
    if (xx(2) /= "ghi") error stop 106
    if (xx(3) /= "jlm") error stop 107
    if (yy(5) /= "def") error stop 105
    if (yy(6) /= "ghi") error stop 106
    if (yy(7) /= "jlm") error stop 107
    if (zz(-k) /= "def") error stop 105
    if (zz(-k+1) /= "ghi") error stop 106
    if (zz(-k+2) /= "jlm") error stop 107
  else
    error stop 108
  endif
  res%x = %loc(xx)  ! { dg-warning "Legacy Extension" }
  res%y = %loc(yy)  ! { dg-warning "Legacy Extension" }
  res%z = %loc(zz)  ! { dg-warning "Legacy Extension" }
end

end module


use m
implicit none (type, external)
character(len=3) :: a(6), a2(6), a3(6), a_init(6)
type(loc_t) :: loc3

a_init = ['abc', 'def', 'ghi', 'jlm', 'nop', 'qrs']

! -- Fortran: assumed size
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_f (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_f (a(2:4), a2(2:4), a3(2:4), size(a(2:4)), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_size_f (n=size(a(2:4)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_in_f (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_in_f (a(2:4), a2(2:4), a3(2:4), size(a(2:4)), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_size_in_f (n=size(a(2:4)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- Fortran: explicit shape
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_f (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_f (a(2:4), a2(2:4), a3(2:4), size(a(::2)), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_expl_size_f (n=size(a(2:4)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_in_f (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_in_f (a(2:4), a2(2:4), a3(2:4), size(a(::2)), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_expl_size_in_f (n=size(a(::2)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- Fortran: assumed rank
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_f (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_rank_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_in_f (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_in_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_rank_in_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- Fortran: assumed rank contiguous
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_f (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_rank_cont_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_in_f (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_in_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_rank_cont_in_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- Fortran: assumed shape
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_f (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_shape_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_in_f (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_in_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_shape_in_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- Fortran: assumed shape contiguous
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_f (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_shape_cont_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_in_f (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_in_f (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_shape_cont_in_f (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2


! --- character - call C directly --

! -- C: assumed size
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_c (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_c (a(2:4), a2(2:4), a3(2:4), size(a(2:4)), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_size_c (n=size(a(2:4)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_in_c (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_size_in_c (a(2:4), a2(2:4), a3(2:4), size(a(2:4)), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_size_in_c (n=size(a(2:4)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- C: explicit shape
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_c (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_c (a(2:4), a2(2:4), a3(2:4), size(a(::2)), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_expl_size_c (n=size(a(::2)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_in_c (a(::2), a2(::2), a3(::2), size(a(::2)), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_expl_size_in_c (a(2:4), a2(2:4), a3(2:4), size(a(::2)), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_expl_size_in_c (n=size(a(::2)), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- C: assumed rank
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_c (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_rank_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_in_c (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_in_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_rank_in_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- C: assumed rank contiguous
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_c (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_rank_cont_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_in_c (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_rank_cont_in_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_rank_cont_in_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- C: assumed shape
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_c (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_shape_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_in_c (a(::2), a2(::2), a3(::2), len(a), num=1)
if (loc3%x /= %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_in_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_shape_in_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

! -- C: assumed shape contiguous
a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_c (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 51  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 51  ! { dg-warning "Legacy Extension" }
if (any (a /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a2 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52
if (any (a3 /= ['ABC', 'def', 'DEF', 'jlm', 'GHI', 'qrs'])) error stop 52

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 53  ! { dg-warning "Legacy Extension" }
if (any (a /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a2 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54
if (any (a3 /= ['abc', 'ABC', 'DEF', 'GHI', 'nop', 'qrs'])) error stop 54

loc3 = char_assumed_shape_cont_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_in_c (a(::2), a2(::2), a3(::2), len(a), num=1)   ! NOTE: run-time copy-in warning
if (loc3%x == %loc(a)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%y == %loc(a2)) error stop 55  ! { dg-warning "Legacy Extension" }
if (loc3%z == %loc(a3)) error stop 55  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 56
if (any (a2 /= a_init)) error stop 56
if (any (a3 /= a_init)) error stop 56

a = a_init; a2 = a_init; a3 = a_init
loc3 = char_assumed_shape_cont_in_c (a(2:4), a2(2:4), a3(2:4), len(a), num=2)
if (loc3%x /= %loc(a(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%y /= %loc(a2(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (loc3%z /= %loc(a3(2))) error stop 57  ! { dg-warning "Legacy Extension" }
if (any (a /= a_init)) error stop 58
if (any (a2 /= a_init)) error stop 58
if (any (a3 /= a_init)) error stop 58

loc3 = char_assumed_shape_cont_in_c (k=len(a), num=3)
if (loc3%x /= -1 .or. loc3%y /= -1 .or. loc3%z /= -1) error stop 2
end

! { dg-output "At line 1003 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_size_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1003 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_size_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1003 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_size_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1024 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_size_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1024 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_size_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1024 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_size_in_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1046 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_expl_size_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1046 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_expl_size_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1046 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_expl_size_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1067 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_expl_size_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1067 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_expl_size_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1067 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_expl_size_in_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1132 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_rank_cont_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1132 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_rank_cont_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1132 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_rank_cont_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1153 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_rank_cont_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1153 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_rank_cont_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1153 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_rank_cont_in_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1218 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_shape_cont_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1218 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_shape_cont_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1218 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_shape_cont_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1239 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_shape_cont_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1239 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_shape_cont_in_f'(\n|\r\n|\r)" }"
! { dg-output "At line 1239 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_shape_cont_in_f'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1264 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_size_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1264 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_size_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1264 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_size_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1285 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_size_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1285 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_size_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1285 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_size_in_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1307 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_expl_size_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1307 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_expl_size_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1307 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_expl_size_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1328 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_expl_size_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1328 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_expl_size_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1328 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_expl_size_in_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1393 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_rank_cont_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1393 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_rank_cont_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1393 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_rank_cont_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1414 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_rank_cont_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1414 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_rank_cont_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1414 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_rank_cont_in_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1479 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_shape_cont_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1479 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_shape_cont_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1479 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_shape_cont_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
! { dg-output "At line 1500 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'xx' of procedure 'char_assumed_shape_cont_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1500 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'yy' of procedure 'char_assumed_shape_cont_in_c'(\n|\r\n|\r)" }"
! { dg-output "At line 1500 of file .*bind-c-contiguous-4.f90(\n|\r\n|\r)" }"
! { dg-output "Fortran runtime warning: An array temporary was created for argument 'zz' of procedure 'char_assumed_shape_cont_in_c'(\n|\r\n|\r)" }"
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defghijlm(\n|\r\n|\r)" }"
