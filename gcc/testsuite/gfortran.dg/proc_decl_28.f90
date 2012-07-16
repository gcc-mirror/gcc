! { dg-do compile }
!
! PR 53956: [F03] PROCEDURE w/ interface: Bogus "EXTERNAL attribute conflicts with FUNCTION attribute"
!
! Contributed by James van Buskirk

  interface
    subroutine sub (a)       
      integer, external :: a
    end subroutine
  end interface

  procedure(sub) :: proc

end 
