! { dg-do compile }
! { dg-options "-O -ftree-vectorize" }
      SUBROUTINE OpenAD_set_ref_state(DRF, RHOFACF, RHOFACC)
      real(8) DRF(1 : 15)
      real(8) RHOFACF(1 : 16)
      real(8) RHOFACC(1 : 15)
      integer, dimension(:), allocatable :: oad_it
      integer :: oad_it_ptr
      INTEGER(8) OpenAD_Symbol_188
      INTEGER(4) K
          OpenAD_Symbol_188 = 0
          DO K = 2, 15, 1
            RHOFACF(INT(K)) = ((RHOFACC(K) * DRF(K + (-1)) + RHOFACC(K +
     + (-1)) * DRF(K)) /(DRF(K) + DRF(K + (-1))))
            OpenAD_Symbol_188 = (INT(OpenAD_Symbol_188) + INT(1))
          END DO
          oad_it(oad_it_ptr) = OpenAD_Symbol_188
      end subroutine OpenAD_set_ref_state
