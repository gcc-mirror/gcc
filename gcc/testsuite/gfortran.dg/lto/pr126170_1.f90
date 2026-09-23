module caller
  use m_ds
contains
  subroutine s()
    type(mdcc) :: self
    type(vs) :: vs_
    vs_ = mdot(self)
  end subroutine s
end module caller

program main
  use caller
  call s()
end program main
