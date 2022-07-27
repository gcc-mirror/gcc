! { dg-do compile }
! { dg-options "-std=legacy -ffree-form -finit-local-zero -finit-derived -fdec-structure" }
!
! PR fortran/105310
!
! Test that gfc_conv_union_initializer does not cause an ICE when called
! to build the constructor for a field which triggers a vector resize.
!

program dec_union_12
  implicit none
STRUCTURE /foo8u/
  ! 8 fields
  INTEGER(4) :: a,b,c,d,e,f,g,h
  UNION
  MAP
  ENDMAP
  ENDUNION
ENDSTRUCTURE
STRUCTURE /foo16u/
  ! 16 fields
  INTEGER(4) :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
  UNION
  MAP
  ENDMAP
  ENDUNION
ENDSTRUCTURE
STRUCTURE /foo32u/
  ! 32 fields
  INTEGER(4) :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
  INTEGER(4) :: aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap
  UNION
  MAP
  ENDMAP
  ENDUNION
ENDSTRUCTURE
  record /foo8u/ bar8u
  record /foo16u/ bar16u
  record /foo32u/ bar32u
  bar8u.a = 1
  bar16u.a = 1
  bar32u.a = 1
end
