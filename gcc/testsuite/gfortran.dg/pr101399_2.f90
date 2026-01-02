! { dg-do compile }
! { dg-options "-Wtabs" }
program foo
   ! The next statement has a tab between 'print' and '*'.
   print	*, 'tab warning'  ! { dg-warning "Nonconforming tab" }
end program foo
