program main
  call test (reshape ((/ 'a', 'b', 'c', 'd' /), (/ 2, 2 /)))
contains
  subroutine test (a)
    character (len = *), dimension (:, :) :: a

    if (size (a, 1) .ne. 2) call abort
    if (size (a, 2) .ne. 2) call abort
    if (len (a) .ne. 1) call abort

    if (a (1, 1) .ne. 'a') call abort
    if (a (2, 1) .ne. 'b') call abort
    if (a (1, 2) .ne. 'c') call abort
    if (a (2, 2) .ne. 'd') call abort
  end subroutine test
end program main
