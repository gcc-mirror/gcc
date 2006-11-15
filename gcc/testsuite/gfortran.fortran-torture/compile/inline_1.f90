program gfcbug43
  call try_fit (1)
  call try_fit (1)
contains
  subroutine try_fit (k)
    call fit (1, debug=.true.)
  end subroutine try_fit
  subroutine fit (k, debug)
    logical,  intent(in),  optional :: debug
    do j = 1, 2
      maxerr1 = funk (r ,x1 , x1)
    end do
    if (debug) then
      print "help"
    end if
  end subroutine fit
end program gfcbug43
