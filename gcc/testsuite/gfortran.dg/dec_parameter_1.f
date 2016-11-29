        ! { dg-do run }
        ! { dg-options "-ffixed-form -std=legacy" }
        !
        ! Test DEC-style PARAMETER statements without parentheses in
        ! fixed form.
        !

        subroutine sub1(t, x, y)
          implicit real(8) (A-H,O-Z)
          parameter   (pi_1 = 3.141592654d0, f_1 = 3.d08)
          parameter    pi_2 = 3.141592654d0, f_2 = 3.d08
          ! Note that if the parameter statements above are matched
          ! incorrectly as assignments, the below specification
          ! statements will be considered out-of-order and we see
          ! 'unexpected specification statement'. A PARAMETER
          ! statement should still be a specification statement.

          real(8), intent(in) :: t
          real(8), intent(out) :: x, y

          real(8), volatile :: two
          two = 2.0d0
          x = two * pi_1 * f_1 * t
          y = two * pi_2 * f_2 * t
          z = two * pi_3 * f_3 * t
          return
        end subroutine

        subroutine sub2(t, x, y, z)
          implicit none
          real(8) :: pi_1, pi_2, f_1, f_2
                   parameter   (pi_1 = 3.141592654d0, f_1 = 3.d08)
                   parameter    pi_2 = 3.141592654d0, f_2 = 3.d08
          real(8), parameter :: pi_3 = 3.141592654d0, f_3 = 3.d08
          ! Ditto sub1

          real(8), intent(in) :: t
          real(8), intent(out) :: x, y, z

          real(8), volatile :: two
          two = 2.0d0
          x = two * pi_1 * f_1 * t
          y = two * pi_2 * f_2 * t
          z = two * pi_3 * f_3 * t
        end subroutine

        implicit none
        real(8) :: x1, x2, y1, y2, z2
        real(8), volatile :: t
        t = 1.5e-6

        call sub1(t, x1, y1)
        call sub2(t, x2, y2, z2)

        write(*,'(4D18.5)') t, x1, y1
        write(*,'(4D18.5)') t, x2, y2, z2

        if (x1 .ne. x2 .or. y1 .ne. y2
     &      .or. x1 .ne. y1 .or. x2 .ne. y2
     &      .or. y2 .ne. z2) then
          call abort()
        endif

        end
