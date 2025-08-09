! { dg-do compile }
!
! Test the F2018 generic statement error reporting of access and name conflicts.
!
! Contributed by Steven Kargl  <kargls@comcast.net>
!
    module foo1

       implicit none
       private

       public bah
       generic :: bah => bah, bak         ! { dg-error "conflicts with that" }

       public bar
       generic :: bar => bah, bak         ! OK - checked that 'bar' is not a procedure

       contains
          integer function bah(i)
             integer, intent(in) :: i
             bah = i
          end function bah
          real function bak(x)
             real, intent(in) :: x
             bak = 42.5
          end function bak
    end module foo1

    module foo2

       implicit none
       private

       generic :: bah => bah, bak   ! { dg-error "conflicts with that" }
       public bah

       generic :: bar => bah, bak   ! OK - checked that 'bar' is not a procedure
       public bar

       contains
          integer function bah(i)
             integer, intent(in) :: i
             bah = i
          end function bah
          real function bak(x)
             real, intent(in) :: x
             bak = 42.5
          end function bak
    end module foo2

    module foo3                     ! { dg-error "clashes with the name of an entity" }

       implicit none
       private

       integer :: bar = 10          ! { dg-error "has a type" }
       generic :: bar => bah, bak   ! { dg-error "has a type" }

       generic :: foo3 => bah, bak  ! { dg-error "clashes with the name of an entity" }

       contains
          integer function bah(i)
             integer, intent(in) :: i
             bah = i
          end function bah
          real function bak(x)
             real, intent(in) :: x
             bak = 42.5
          end function bak
    end module foo3

    module foo4
        implicit none
        private
        public bak

        generic :: bak => bar, bah

    contains
        function bar(i)
            real bar
            integer, intent(in) :: i
            bar = i
        end function bar
        function bah(x)
            real bah
            real, intent(in) :: x
            bah = x
        end function bah
    end module foo4

    program snooze
        use foo4
        print *, bak(42)   ! Public statement for 'bak' exposes the
        print *, bak(43.5) ! specific procedures 'bar' and 'bah' here.
    end program snooze
