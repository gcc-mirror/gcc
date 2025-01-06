! { dg-do compile { target aarch64-*-* } }
! { dg-additional-options "-w -Ofast" }

  module brute_force
    integer, parameter :: r=9
     integer  block(r, r, 0)
    contains
  subroutine brute
     do
      do
          do
           do
                do
                     do
                         do i7 = l0, 1
                       select case(1 )
                       case(1)
                           block(:2, 7:, 1) = block(:2, 7:, i7) - 1
                       end select
                            do i8 = 1, 1
                               do i9 = 1, 1
                            if(1 == 1) then
                                    call digits_20
                                end if
                                end do
                          end do
                    end do
                    end do
              end do
              end do
           end do
     end do
  end do
 end
  end

! { dg-final { scan-assembler-not {ldr\s+d([0-9]+),\s+\[x[0-9]+, x[0-9]+\]} } }
