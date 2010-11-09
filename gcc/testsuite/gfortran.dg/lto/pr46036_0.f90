! { dg-lto-do link }
! { dg-lto-options {{ -O -flto -ftree-vectorize }} }

function no_of_edges(self) result(res)
  integer(kind=kind(1)) :: edge_bit_string
  integer(kind=kind(1)) :: res
  integer(kind=kind(1)) :: e
  do e = 0, 11
     if (.not. btest(edge_bit_string,e)) cycle
     res = res + 1
  end do
end function no_of_edges

end program
