! { dg-do run }
! PR 17612
! We used to not determine the length of character-valued expressions
! correctly, leading to a segfault.
program prog
  character(len=2), target :: c(4)
  type pseudo_upf 
     character(len=2), pointer :: els(:)
  end type pseudo_upf
  type (pseudo_upf) :: p
  type t
    character(5) :: s(2)
  end type
  type (t) v
  ! A full arrays.
  c = (/"ab","cd","ef","gh"/)
  call n(p)
  if (any (c /= p%els)) call abort
  ! An array section that needs a new array descriptor.
  v%s(1) = "hello"
  v%s(2) = "world"
  call test (v%s)
contains 

  subroutine n (upf) 
    type (pseudo_upf), intent(inout) :: upf 
    upf%els => c
    return 
  end subroutine n

  subroutine test(s)
    character(len=*) :: s(:)
    if ((len (s) .ne. 5) .or. (any (s .ne. (/"hello", "world"/)))) call abort
  end subroutine
end program
  

