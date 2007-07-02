! { dg-do run }
subroutine display() bind(c)
  implicit none
end subroutine display

program main
  implicit none
  interface
     subroutine display() bind(c)
     end subroutine display
  end interface
end program main
