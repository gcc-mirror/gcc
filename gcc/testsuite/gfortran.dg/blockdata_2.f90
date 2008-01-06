! { dg-do compile }
! Test for pr29537 where we did ICE trying to dereference the NULL
! proc_name from an unnamed block data which we intended to use as locus
! for a blank common.
block data
  common c
end !block data
end
