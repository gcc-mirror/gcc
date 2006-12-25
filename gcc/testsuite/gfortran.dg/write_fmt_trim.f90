! { dg-do run }
! PR30200 write(*,myfmt="(1X,a,'xyz')") "A"  prints  Az' instead of Axyz
! Test case from PR, submitted by <jvdelisle@gcc.gnu.org>
program main
  character (len=20) format
  format = "(1X,a,'xyz')"
  write(*,fmt=trim(format)) "A"  ! Problem arose when trim was included here
end
! { dg-output " Axyz" }

