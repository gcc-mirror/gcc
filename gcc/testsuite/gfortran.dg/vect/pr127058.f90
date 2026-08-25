! { dg-do compile }
! { dg-additional-options "-Ofast" }
! { dg-additional-options "-mcpu=neoverse-v1" { target { aarch64-*-* } } }

MODULE a
    integer, parameter ::      b = selected_real_kind(12)
    integer, parameter :: d     =  10
    real(b) e(1,d)
    CONTAINS
    SUBROUTINE f(ime ,jme)
    REAL, DIMENSION( ime,d,jme )  :: g
    INTEGER  c,h,l,k,aa,ab
        DO h = jts,jte
        DO l = its,aa
        if (m.eq.1) THEN
           do c = 1,ab
            do k = 1,d
               e(c,k) = g(i,k,j)
            enddo
            enddo
        endif
        end do
    end do
    end
end
