! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Test a regression where union components could compare equal to structure/map
! components, causing an ICE in gfc_conv_component_ref.
!

implicit none

structure /s1/
  integer(4) i
end structure

structure /s2/
  union
    map
      record /s1/ r
    end map
  end union
end structure

record /s2/ x

x.r.i = 0

end
