! { dg-do run }
  character(len=1) :: s
  character(len=0) :: s0
  s = " "
  s0 = ""
  call bar ("")
  call bar (s)
  call bar (s0)
  call bar (trim(s))
  call bar (min(s0,s0))
contains
  subroutine bar (s)
    character(len=*), optional :: s
    if (.not. present (S)) call abort
  end subroutine bar
end
