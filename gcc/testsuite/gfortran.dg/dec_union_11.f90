! { dg-do compile }
! { dg-options "-g -fdec-structure" }
!
! Test a regression where typespecs of unions containing character buffers of
! different lengths where copied, resulting in a bad gimple tree state.
!

subroutine sub2 (otherbuf)
  integer, parameter :: L_bbuf = 65536
  integer, parameter :: L_bbuf2 = 24

  structure /buffer2/
    union
     map
      character(L_bbuf2)  sbuf
     end map
    end union
  end structure
  structure /buffer/
    union
     map
      character(L_bbuf)  sbuf
     end map
    end union
  end structure

  record /buffer/ buf1
  record /buffer2/ buf2
  common /c/ buf1, buf2

  record /buffer2/ otherbuf
end subroutine

subroutine sub()
  integer, parameter :: L_bbuf = 65536
  integer, parameter :: L_bbuf2 = 24

  structure /buffer2/
    union
     map
      character(L_bbuf2)  sbuf
     end map
    end union
  end structure
  structure /buffer/
    union
     map
      character(L_bbuf)  sbuf
     end map
    end union
  end structure

  record /buffer/ buf1
  record /buffer2/ buf2
  common /c/ buf1, buf2

  call sub2 (buf1) ! { dg-warning "Type mismatch" }
  return
end subroutine

call sub()

end
