! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! PR fortran/77782
!
! Test an ICE where a union might be considered equal to a structure,
! causing the union's backend_decl to be replaced with that of the structure.
!

program p

structure /s1/
  union
    map
      integer(4) a
      end map
    map
      real(4) b
    end map
  end union
end structure

structure /s2/
  union ! regression: if this union == s1, we ICE in gfc_get_union_type
    map
      integer(2) x, y
      integer(4) z
    end map
  end union
end structure

record /s1/ r1
r1.a = 0

end
