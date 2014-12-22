! { dg-do compile }
! { dg-options "-fcoarray=single -fmax-errors=80" }
!
!
! CO_REDUCE (plus CO_MIN/MAX/SUM/BROADCAST)
!
program test
  implicit none (external, type)
  intrinsic co_reduce
  intrinsic co_broadcast
  intrinsic co_min
  intrinsic co_max
  intrinsic co_sum
  intrinsic dprod
  external ext

  type t
    procedure(), pointer, nopass :: ext
    procedure(valid), pointer, nopass :: valid
    procedure(sub), pointer, nopass :: sub
    procedure(nonpure), pointer, nopass :: nonpure
    procedure(arg1), pointer, nopass :: arg1
    procedure(arg3), pointer, nopass :: arg3
    procedure(elem), pointer, nopass :: elem
    procedure(realo), pointer, nopass :: realo
    procedure(int8), pointer, nopass :: int8
    procedure(arr), pointer, nopass :: arr
    procedure(ptr), pointer, nopass :: ptr
    procedure(alloc), pointer, nopass :: alloc
    procedure(opt), pointer, nopass :: opt
    procedure(val), pointer, nopass :: val
    procedure(async), pointer, nopass :: async
    procedure(tgt), pointer, nopass :: tgt
    procedure(char44), pointer, nopass :: char44
    procedure(char34), pointer, nopass :: char34
  end type t

  type(t) :: dt
  integer :: caf[*]
  character(len=3) :: c3
  character(len=4) :: c4



  call co_min(caf[1]) ! { dg-error "shall not be coindexed" }
  call co_max(caf[1]) ! { dg-error "shall not be coindexed" }
  call co_sum(caf[1]) ! { dg-error "shall not be coindexed" }
  call co_broadcast(caf[1], source_image=1) ! { dg-error "shall not be coindexed" }
  call co_reduce(caf[1], valid) ! { dg-error "shall not be coindexed" }

  call co_reduce(caf, valid) ! OK
  call co_reduce(caf, dt%valid) ! OK
  call co_reduce(caf, dprod) ! { dg-error "is not permitted for CO_REDUCE" }
  call co_reduce(caf, ext) ! { dg-error "must be a PURE function" }
  call co_reduce(caf, dt%ext) ! { dg-error "must be a PURE function" }
  call co_reduce(caf, sub) ! { dg-error "must be a PURE function" }
  call co_reduce(caf, dt%sub) ! { dg-error "must be a PURE function" }
  call co_reduce(caf, nonpure) ! { dg-error "must be a PURE function" }
  call co_reduce(caf, dt%nonpure) ! { dg-error "must be a PURE function" }
  call co_reduce(caf, arg1) ! { dg-error "shall have two arguments" }
  call co_reduce(caf, dt%arg1) ! { dg-error "shall have two arguments" }
  call co_reduce(caf, arg3) ! { dg-error "shall have two arguments" }
  call co_reduce(caf, dt%arg3) ! { dg-error "shall have two arguments" }
  call co_reduce(caf, elem) ! { dg-error "ELEMENTAL non-INTRINSIC procedure 'elem' is not allowed as an actual argument" }
  call co_reduce(caf, dt%elem) ! { dg-error "ELEMENTAL procedure pointer component 'elem' is not allowed as an actual argument" }
  call co_reduce(caf, realo) ! { dg-error "A argument at .1. has type INTEGER.4. but the function passed as OPERATOR at .2. returns REAL.4." }
  call co_reduce(caf, dt%realo) ! { dg-error "A argument at .1. has type INTEGER.4. but the function passed as OPERATOR at .2. returns REAL.4." }
  call co_reduce(caf, int8) ! { dg-error "A argument at .1. has type INTEGER.4. but the function passed as OPERATOR at .2. returns INTEGER.8." }
  call co_reduce(caf, dt%int8) ! { dg-error "A argument at .1. has type INTEGER.4. but the function passed as OPERATOR at .2. returns INTEGER.8." }
  call co_reduce(caf, arr) ! { dg-error "scalar nonallocatable nonpointer arguments and return a nonallocatable nonpointer scalar" }
  call co_reduce(caf, dt%arr) ! { dg-error "scalar nonallocatable nonpointer arguments and return a nonallocatable nonpointer scalar" }
  call co_reduce(caf, ptr) ! { dg-error "scalar nonallocatable nonpointer arguments and return a nonallocatable nonpointer scalar" }
  call co_reduce(caf, dt%ptr) ! { dg-error "scalar nonallocatable nonpointer arguments and return a nonallocatable nonpointer scalar" }
  call co_reduce(caf, alloc) ! { dg-error "scalar nonallocatable nonpointer arguments and return a nonallocatable nonpointer scalar" }
  call co_reduce(caf, dt%alloc) ! { dg-error "scalar nonallocatable nonpointer arguments and return a nonallocatable nonpointer scalar" }
  call co_reduce(caf, opt) ! { dg-error "shall not have the OPTIONAL attribute for either of the arguments" }
  call co_reduce(caf, dt%opt) ! { dg-error "shall not have the OPTIONAL attribute for either of the arguments" }
  call co_reduce(caf, val) ! { dg-error "shall have the VALUE attribute either for none or both arguments" }
  call co_reduce(caf, dt%val) ! { dg-error "shall have the VALUE attribute either for none or both arguments" }
  call co_reduce(caf, async) ! { dg-error "shall have the ASYNCHRONOUS attribute either for none or both arguments" }
  call co_reduce(caf, dt%async) ! { dg-error "shall have the ASYNCHRONOUS attribute either for none or both arguments" }
  call co_reduce(caf, tgt) ! { dg-error "shall have the TARGET attribute either for none or both arguments" }
  call co_reduce(caf, dt%tgt) ! { dg-error "shall have the TARGET attribute either for none or both arguments" }
  call co_reduce(c4, char44) ! OK
  call co_reduce(c4, dt%char44) ! OK
  call co_reduce(c3, char34) ! { dg-error "character length of the A argument at .1. and of the arguments of the OPERATOR at .2. shall be the same" }
  call co_reduce(c3, dt%char34) ! { dg-error "character length of the A argument at .1. and of the arguments of the OPERATOR at .2. shall be the same" }
  call co_reduce(c4, char34) ! { dg-error "The character length of the A argument at .1. and of the function result of the OPERATOR at .2. shall be the same" }
  call co_reduce(c4, dt%char34) ! { dg-error "The character length of the A argument at .1. and of the function result of the OPERATOR at .2. shall be the same" }

contains
  pure integer function valid(x,y)
    integer, value :: x, y
  end function valid
  impure integer function nonpure(x,y)
    integer, value :: x, y
  end function nonpure
  pure subroutine sub()
  end subroutine sub
  pure integer function arg3(x, y, z)
    integer, value :: x, y, z
  end function arg3
  pure integer function arg1(x)
    integer, value :: x
  end function arg1
  pure elemental integer function elem(x,y)
    integer, value :: x, y
  end function elem
  pure real function realo(x,y)
    integer, value :: x, y
  end function realo
  pure integer(8) function int8(x,y)
    integer, value :: x, y
  end function int8
  pure integer function arr(x,y)
    integer, intent(in) :: x(:), y
  end function arr
  pure integer function ptr(x,y)
    integer, intent(in), pointer :: x, y
  end function ptr
  pure integer function alloc(x,y)
    integer, intent(in), allocatable :: x, y
  end function alloc
  pure integer function opt(x,y)
    integer, intent(in) :: x, y
    optional :: x, y
  end function opt
  pure integer function val(x,y)
    integer, value :: x
    integer, intent(in) :: y
  end function val
  pure integer function tgt(x,y)
    integer, intent(in) :: x, y
    target :: x
  end function tgt
  pure integer function async(x,y)
    integer, intent(in) :: x, y
    asynchronous :: y
  end function async
  pure character(4) function char44(x,y)
    character(len=4), value :: x, y
  end function char44
  pure character(3) function char34(x,y)
    character(len=4), value :: x, y
  end function char34
end program test
