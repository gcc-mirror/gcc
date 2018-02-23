! Testcases for the AND, OR and XOR functions (GNU intrinsics).
! { dg-do run }
! { dg-options "-ffixed-line-length-none" }
      integer(kind=1) i1, j1
      integer(kind=2) i2, j2
      integer         i4, j4
      integer(kind=8) i8, j8
      logical(kind=1) l1, k1
      logical(kind=2) l2, k2
      logical         l4, k4
      logical(kind=8) l8, k8

#define TEST_INTEGER(u,ukind,v,vkind) \
      ukind = u;\
      vkind = v;\
      if (iand(u,v) /= and(ukind, vkind)) STOP 1;\
      if (iand(u,v) /= and(vkind, ukind)) STOP 1;\
      if (ieor(u,v) /= xor(ukind, vkind)) STOP 1;\
      if (ieor(u,v) /= xor(vkind, ukind)) STOP 1;\
      if (ior(u,v) /= or(ukind, vkind)) STOP 1;\
      if (ior(u,v) /= or(vkind, ukind)) STOP 1

      TEST_INTEGER(19,i1,6,j1)
      TEST_INTEGER(19,i1,6,j2)
      TEST_INTEGER(19,i1,6,j4)
      TEST_INTEGER(19,i1,6,j8)

      TEST_INTEGER(19,i2,6,j1)
      TEST_INTEGER(19,i2,6,j2)
      TEST_INTEGER(19,i2,6,j4)
      TEST_INTEGER(19,i2,6,j8)

      TEST_INTEGER(19,i4,6,j1)
      TEST_INTEGER(19,i4,6,j2)
      TEST_INTEGER(19,i4,6,j4)
      TEST_INTEGER(19,i4,6,j8)

      TEST_INTEGER(19,i8,6,j1)
      TEST_INTEGER(19,i8,6,j2)
      TEST_INTEGER(19,i8,6,j4)
      TEST_INTEGER(19,i8,6,j8)



#define TEST_LOGICAL(u,ukind,v,vkind) \
      ukind = u;\
      vkind = v;\
      if ((u .and. v) .neqv. and(ukind, vkind)) STOP 1;\
      if ((u .and. v) .neqv. and(vkind, ukind)) STOP 1;\
      if (((u .and. .not. v) .or. (.not. u .and. v)) .neqv. xor(ukind, vkind)) STOP 1;\
      if (((u .and. .not. v) .or. (.not. u .and. v)) .neqv. xor(vkind, ukind)) STOP 1;\
      if ((u .or. v) .neqv. or(ukind, vkind)) STOP 1;\
      if ((u .or. v) .neqv. or(vkind, ukind)) STOP 2

      TEST_LOGICAL(.true.,l1,.false.,k1)
      TEST_LOGICAL(.true.,l1,.true.,k1)
      TEST_LOGICAL(.true.,l1,.false.,k2)
      TEST_LOGICAL(.true.,l1,.true.,k2)
      TEST_LOGICAL(.true.,l1,.false.,k4)
      TEST_LOGICAL(.true.,l1,.true.,k4)
      TEST_LOGICAL(.true.,l1,.false.,k8)
      TEST_LOGICAL(.true.,l1,.true.,k8)

      TEST_LOGICAL(.true.,l2,.false.,k1)
      TEST_LOGICAL(.true.,l2,.true.,k1)
      TEST_LOGICAL(.true.,l2,.false.,k2)
      TEST_LOGICAL(.true.,l2,.true.,k2)
      TEST_LOGICAL(.true.,l2,.false.,k4)
      TEST_LOGICAL(.true.,l2,.true.,k4)
      TEST_LOGICAL(.true.,l2,.false.,k8)
      TEST_LOGICAL(.true.,l2,.true.,k8)

      TEST_LOGICAL(.true.,l4,.false.,k1)
      TEST_LOGICAL(.true.,l4,.true.,k1)
      TEST_LOGICAL(.true.,l4,.false.,k2)
      TEST_LOGICAL(.true.,l4,.true.,k2)
      TEST_LOGICAL(.true.,l4,.false.,k4)
      TEST_LOGICAL(.true.,l4,.true.,k4)
      TEST_LOGICAL(.true.,l4,.false.,k8)
      TEST_LOGICAL(.true.,l4,.true.,k8)

      TEST_LOGICAL(.true.,l8,.false.,k1)
      TEST_LOGICAL(.true.,l8,.true.,k1)
      TEST_LOGICAL(.true.,l8,.false.,k2)
      TEST_LOGICAL(.true.,l8,.true.,k2)
      TEST_LOGICAL(.true.,l8,.false.,k4)
      TEST_LOGICAL(.true.,l8,.true.,k4)
      TEST_LOGICAL(.true.,l8,.false.,k8)
      TEST_LOGICAL(.true.,l8,.true.,k8)

      end
