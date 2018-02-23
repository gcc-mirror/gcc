program main
  call test (reshape ((/ 'a', 'b', 'c', 'd' /), (/ 2, 2 /)))
contains
  subroutine test (a)
    character (len = *), dimension (:, :) :: a

    if (size (a, 1) .ne. 2) STOP 1
    if (size (a, 2) .ne. 2) STOP 2
    if (len (a) .ne. 1) STOP 3

    if (a (1, 1) .ne. 'a') STOP 4
    if (a (2, 1) .ne. 'b') STOP 5
    if (a (1, 2) .ne. 'c') STOP 6
    if (a (2, 2) .ne. 'd') STOP 7
  end subroutine test
end program main
