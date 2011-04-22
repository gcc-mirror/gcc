! { dg-do compile }
! { dg-options "-O -fdump-tree-original" }
! PR 48405 - function elimnination in a DO loop should work.
program main
  interface
     pure function mypure()
       integer :: mypure
     end function mypure
  end interface
  DO I=1,mypure() + mypure()
  ENDDO
END program main
! { dg-final { scan-tree-dump-times "mypure" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }


