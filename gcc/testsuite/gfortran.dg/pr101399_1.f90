! { dg-do compile }
! { dg-options "-O0" }
program foo
   ! The next statement has a tab between 'print' and '*'.
   print	*, 'tab not always ignored'
end program foo
