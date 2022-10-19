! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple -fdump-tree-original" }

! Check that 'omp depobj's depend and 'omp task/... depend' depend on
! the same variable

! For pointers, it depends on the address of the pointer target
! For allocatable, on the allocated memory address

subroutine foo(dss, dsp, dsa, daa, daaa, daap, doss, dosp, dosa, doaa, doaaa, doaap, &
               dssv, dossv)
  !use omp_lib
  use iso_c_binding, only: c_intptr_t
  implicit none (type, external)
  integer, parameter :: omp_depend_kind = 2*c_intptr_t
  integer :: ss, sp, sa, aa(4), aaa(:), aap(:)
  integer :: dss, dsp, dsa, daa(4), daaa(:), daap(:)
  integer :: doss, dosp, dosa, doaa(4), doaaa(:), doaap(:)
  optional :: doss, dosp, dosa, doaa, doaaa, doaap
  allocatable :: sa, aaa, dsa, daaa, dosa, doaaa
  pointer :: sp, aap, dsp, daap, dosp, doaap
  integer, value :: dssv, dossv
  optional :: dossv

  integer(omp_depend_kind) :: object(20)
  integer(omp_depend_kind) :: elem(9)

  !$omp depobj(object(1)) depend(in: ss)
  !$omp depobj(object(2)) depend(in: sp)
  !$omp depobj(object(3)) depend(in: sa)
  !$omp depobj(object(4)) depend(in: aa)
  !$omp depobj(object(5)) depend(in: aaa)
  !$omp depobj(object(6)) depend(in: aap)
  !$omp depobj(object(7)) depend(in: dss)
  !$omp depobj(object(8)) depend(in: dsp)
  !$omp depobj(object(9)) depend(in: dsa)
  !$omp depobj(object(10)) depend(in: daa)
  !$omp depobj(object(11)) depend(in: daaa)
  !$omp depobj(object(12)) depend(in: daap)
  !$omp depobj(object(13)) depend(in: doss)
  !$omp depobj(object(14)) depend(in: dosp)
  !$omp depobj(object(15)) depend(in: dosa)
  !$omp depobj(object(16)) depend(in: doaa)
  !$omp depobj(object(17)) depend(in: doaaa)
  !$omp depobj(object(18)) depend(in: doaap)
  !$omp depobj(object(19)) depend(in: dssv)
  !$omp depobj(object(20)) depend(in: dossv)

  !$omp depobj(elem(1)) depend(in: aa(2))
  !$omp depobj(elem(2)) depend(in: aaa(2))
  !$omp depobj(elem(3)) depend(in: aap(2))
  !$omp depobj(elem(4)) depend(in: daa(2))
  !$omp depobj(elem(5)) depend(in: daaa(2))
  !$omp depobj(elem(6)) depend(in: daap(2))
  !$omp depobj(elem(7)) depend(in: doaa(2))
  !$omp depobj(elem(8)) depend(in: doaaa(2))
  !$omp depobj(elem(9)) depend(in: doaap(2))

  !$omp parallel
  !$omp single
    !$omp task depend(out: ss)
      ss = 4
    !$omp end task
    !$omp task depend(out: sp)
      sp = 4
    !$omp end task
    !$omp task depend(out: sa)
      sa = 4
    !$omp end task
    !$omp task depend(out: aa)
      aa = 4
    !$omp end task
    !$omp task depend(out: aaa)
      aaa = 4
    !$omp end task
    !$omp task depend(out: aap)
      aap = 4
    !$omp end task
    !$omp task depend(out: dss)
      dss = 4
    !$omp end task
    !$omp task depend(out: dsp)
      dsp = 4
    !$omp end task
    !$omp task depend(out: dsa)
      dsa = 4
    !$omp end task
    !$omp task depend(out: daa)
      daa = 4
    !$omp end task
    !$omp task depend(out: daaa)
      daaa = 4
    !$omp end task
    !$omp task depend(out: daap)
      daap = 4
    !$omp end task
    !$omp task depend(out: doss)
      doss = 4
    !$omp end task
    !$omp task depend(out: dosp)
      dosp = 4
    !$omp end task
    !$omp task depend(out: dosa)
      dosa = 4
    !$omp end task
    !$omp task depend(out: doaa)
      doaa = 4
    !$omp end task
    !$omp task depend(out: doaaa)
      doaaa = 4
    !$omp end task
    !$omp task depend(out: doaap)
      doaap = 4
    !$omp end task
    !$omp task depend(out: dossv)
      dossv = 4
    !$omp end task
    !$omp task depend(out: dssv)
      dssv = 4
    !$omp end task

    !$omp task depend(out: aa(2))
      aa(2) = 4
    !$omp end task
    !$omp task depend(out: aaa(2))
      aaa(2) = 4
    !$omp end task
    !$omp task depend(out: aap(2))
      aap(2) = 4
    !$omp end task
    !$omp task depend(out: daa(2))
      daa(2) = 4
    !$omp end task
    !$omp task depend(out: daaa(2))
      daaa(2) = 4
    !$omp end task
    !$omp task depend(out: daap(2))
      daap(2) = 4
    !$omp end task
    !$omp task depend(out: doaa(2))
      doaa(2) = 4
    !$omp end task
    !$omp task depend(out: doaaa(2))
      doaaa(2) = 4
    !$omp end task
    !$omp task depend(out: doaap(2))
      doaap(2) = 4
    !$omp end task
  !$omp end single
  !$omp end parallel
end

subroutine bar
  implicit none (type, external)
  integer :: depvar, x

  x = 7
  !$omp parallel
  !$omp single
    !$omp task depend(out: depvar)
      x =5
    !$omp end task
    !$omp task depend(in: depvar)
      if (x /= 5) stop
    !$omp end task
  !$omp end single
  !$omp end parallel
end

! depvar - only used for dependency, but should still be used in depend:

! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:depvar\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(in:depvar\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "&object\\\[0\\\] = &ss;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[1\\\] = sp;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[2\\\] = sa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[3\\\] = &aa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[4\\\] = .integer.kind=4.\\\[0:\\\] \\* restrict\\) aaa.data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[5\\\] = .integer.kind=4.\\\[0:\\\] \\*\\) aap.data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[6\\\] = dss;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[7\\\] = \\*dsp;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[8\\\] = \\*dsa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[9\\\] = daa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[10\\\] = .integer.kind=4.\\\[0:\\\] \\* restrict\\) daaa->data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[11\\\] = .integer.kind=4.\\\[0:\\\] \\*\\) daap->data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[12\\\] = doss;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[13\\\] = \\*dosp;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[14\\\] = \\*dosa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[15\\\] = doaa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[16\\\] = .integer.kind=4.\\\[0:\\\] \\* restrict\\) doaaa->data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[17\\\] = .integer.kind=4.\\\[0:\\\] \\*\\) doaap->data;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[18\\\] = &dssv;" 1 "original" } }
! { dg-final { scan-tree-dump-times "&object\\\[19\\\] = &dossv;" 1 "original" } }

! { dg-final { scan-tree-dump-times "&elem\\\[0\\\] = &aa\\\[1\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[1\\\] = &\\(\\*\\(integer.kind=4.\\\[0:\\\] \\* restrict\\) aaa.data\\)\\\[aaa.offset \\+ 2\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[2\\\] = \\(integer.kind=4. \\*\\) \\(aap.data \\+ .sizetype. \\(\\(aap.offset \\+ aap.dim\\\[0\\\].stride \\* 2\\) \\* aap.span\\)\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[3\\\] = &\\(\\*daa\\)\\\[1\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[4\\\] = &\\(\\*\\(integer.kind=4.\\\[0:\\\] \\* restrict\\) daaa->data\\)\\\[daaa->offset \\+ 2\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[5\\\] = \\(integer.kind=4. \\*\\) \\(daap->data \\+ .sizetype. \\(\\(daap->offset \\+ daap->dim\\\[0\\\].stride \\* 2\\) \\* daap->span\\)\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[6\\\] = &\\(\\*doaa\\)\\\[1\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[7\\\] = &\\(\\*\\(integer.kind=4.\\\[0:\\\] \\* restrict\\) doaaa->data\\)\\\[doaaa->offset \\+ 2\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "&elem\\\[8\\\] = \\(integer.kind=4. \\*\\) \\(doaap->data \\+ .sizetype. \\(\\(doaap->offset \\+ doaap->dim\\\[0\\\].stride \\* 2\\) \\* doaap->span\\)\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:ss\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*sp\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*sa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:aa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) aaa.data\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) aap.data\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*dss\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\*dsp\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\*dsa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*daa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) daaa->data\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) daap->data\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*doss\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\*dosp\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\*dosa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*doaa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) doaaa->data\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) doaap->data\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:aa\\\[1\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) aaa.data\\)\\\[aaa.offset \\+ 2\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\) \\*\\) \\(aap.data \\+ \\(sizetype\\) \\(\\(aap.offset \\+ aap.dim\\\[0\\\].stride \\* 2\\) \\* aap.span\\)\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\(\\*daa\\)\\\[1\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) daaa->data\\)\\\[daaa->offset \\+ 2\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\) \\*\\) \\(daap->data \\+ \\(sizetype\\) \\(\\(daap->offset \\+ daap->dim\\\[0\\\].stride \\* 2\\) \\* daap->span\\)\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\(\\*doaa\\)\\\[1\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) doaaa->data\\)\\\[doaaa->offset \\+ 2\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:\\*\\(integer\\(kind=4\\) \\*\\) \\(doaap->data \\+ \\(sizetype\\) \\(\\(doaap->offset \\+ doaap->dim\\\[0\\\].stride \\* 2\\) \\* doaap->span\\)\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:dossv\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:dssv\\)" 1 "original" } }


! gimple dump - check only those which are simple one-line checkable:

! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:&ss\\) shared\\(ss\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:sp\\) shared\\(sp\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:sa\\) shared\\(sa\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:&aa\\) shared\\(aa\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:dss\\) shared\\(dss\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:daa\\) shared\\(daa\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:doss\\) shared\\(doss\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:doaa\\) shared\\(doaa\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:&aa\\\[1\\\]\\) shared\\(aa\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:&dossv\\) shared\\(dossv\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(out:&dssv\\) shared\\(dssv\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = \\*dsp;" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = \\*dsa;" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = \\*dosp;" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = \\*dosa;" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = doaaa->data;" 4 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = doaap->data;" 4 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = &\\(\\*daa\\)\\\[1\\\];" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = &\\(\\*doaa\\)\\\[1\\\];" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "= &dssv;" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "= &dossv;" 1 "gimple" } }


