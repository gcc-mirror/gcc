! { dg-do assemble }
module n
private u
contains
  subroutine u
  end subroutine u
end module n
module m
  private :: u
contains
  subroutine u
  end subroutine u
end module m
