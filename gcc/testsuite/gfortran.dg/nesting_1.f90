! PR 18525
! we used to incorrectly refer to n from a when resolving the call to
! c from b
! { dg-do run }
subroutine a(n)
call b(n+1)
contains
  subroutine b(n)
    call c(n)
  end subroutine b

  subroutine c(m)
    if (m/=1) call abort
  end subroutine c
end subroutine a

call a(0)
end
