! { dg-do run { target { ! { *-*-mingw* *-*-cygwin* spu-*-* } } } }
! { dg-options "-std=gnu" }
! See PR38956.  Test fails on cygwin when user has Administrator rights
  implicit none
  character(len=*), parameter :: n = "foobar_file_chmod_2"
  integer :: i

  open (10,file=n)
  close (10,status="delete")

  open (10,file=n)
  close (10,status="keep")

  if (access(n,"") /= 0 .or. access(n," ") /= 0 .or. access(n,"r") /= 0 .or. &
      access(n,"R") /= 0 .or. access(n,"w") /= 0 .or. access(n,"W") /= 0) &
    call abort

  i = chmod (n, "a+x")
  if (i == 0) then
    if (access(n,"x") /= 0 .or. access(n,"X") /= 0) call abort
  end if

  i = chmod (n, "a-w")
  if (i == 0 .and. getuid() /= 0) then
    if (access(n,"w") == 0 .or. access(n,"W") == 0) call abort
  end if

  open (10,file=n)
  close (10,status="delete")

  if (access(n,"") == 0 .or. access(n," ") == 0 .or. access(n,"r") == 0 .or. &
      access(n,"R") == 0 .or. access(n,"w") == 0 .or. access(n,"W") == 0) &
    call abort

  end
