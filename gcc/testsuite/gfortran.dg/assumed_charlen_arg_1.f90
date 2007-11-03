! { dg-do run }
! From PR 33881
  call create_watch_ss(" ")
contains
  subroutine create_watch_actual(name)
    character(len=1) :: name(1)
  end subroutine create_watch_actual

  subroutine create_watch_ss(name,clock)
    character(len=*) :: name
    integer, optional :: clock
    if (present(clock)) then
      call create_watch_actual((/name/))
    else
      call create_watch_actual((/name/))
    end if
  end subroutine create_watch_ss
end
