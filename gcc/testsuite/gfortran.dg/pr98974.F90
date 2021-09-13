! PR middle-end/98974
! { dg-do compile }
! { dg-options "-Ofast" }
! { dg-additional-options "-mcpu=neoverse-v1" { target aarch64*-*-* } }

module module_foobar
  integer,parameter :: fp_kind = selected_real_kind(15)
contains
 subroutine foobar( foo, ix ,jx ,kx,iy,ky)
   real, dimension( ix, kx, jx )  :: foo
   real(fp_kind), dimension( iy, ky, 3 ) :: bar, baz
       do k=1,ky
          do i=1,iy
                if ( baz(i,k,1) > 0. ) then
                  bar(i,k,1) = 0
                endif
                foo(i,nk,j) = baz0 *  bar(i,k,1)
          enddo
       enddo
 end
end
