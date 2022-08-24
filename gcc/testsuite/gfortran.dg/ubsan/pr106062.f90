! PR c++/106062
! { dg-do compile }
! { dg-options "-O2 -fsanitize=undefined" }

call test (reshape ((/ 'a', 'b', 'c', 'd' /), (/ 2, 2 /)))
contains
  subroutine test (a)
    character (*), dimension (:, :) :: a
    if (len (a) .ne. 1) STOP 
  end  
end
