! { dg-do compile }
! { dg-options "-march=rv64gcv -mabi=lp64d -Ofast -fallow-argument-mismatch -fmax-stack-var-size=65536 -S  -std=legacy -w" }

module a
  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12)
end module a
module b
  use a,  c => shr_kind_r8
contains
  subroutine d(cg , km, i1, i2)
    real (c) ch(i2,km)
    real (c) cg(4,i1:i2,km)
    real  dc(i2,km)
    real(c) ci(i2,km)
    real(c) cj(i2,km)
    do k=2,ck
       do i=i1,0
          cl = ci(i,k) *ci(i,1) /      cj(i,k)+ch(i,1)
          cm = cg(1,i,k) - min(e,cg(1,i,co))
          dc(i,k) = sign(cm, cl)
       enddo
    enddo
    if ( cq == 0 ) then
       do i=i1,i2
          if( cr <=  cs ) then
             cg= sign( min(ct,   cg),  cg)
          endif
       enddo
    endif
  end subroutine d
end module b
