! { dg-do compile }
! { dg-options "-fdump-tree-original" }

program test

  character(kind=1,len=10) :: s1 = 4_"foobargee", t1 = 4_""
  character(kind=4,len=10) :: s4 = "foobargee", t4 = ""

  t1(5:5) = s1(6:6)
  t4(5:5) = s4(6:6)
  t4(5:5) = s1(6:6)
  t1(5:5) = s4(6:6)

  call sub (t1, t4)

end program test

! { dg-final { scan-tree-dump-times "memmove" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
