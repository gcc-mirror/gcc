// PR debug/100541
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-tree-dce -fno-tree-dominator-opts -fcompare-debug" }

int INVALID_TILE;
void GetSlopePixelZ();
long RUNWAY_IN_block;
struct Airport {
  long flags;
};
struct Station {
  Airport airport;
} * HandleCrashedAircraft_st;
short HandleCrashedAircraft_v_0;
void HandleCrashedAircraft() {
  Station *__trans_tmp_1 = INVALID_TILE ? nullptr : HandleCrashedAircraft_st,
          *st;
  if (HandleCrashedAircraft_v_0 && __trans_tmp_1 == nullptr &&
      HandleCrashedAircraft_v_0 % 3)
    GetSlopePixelZ();
  if (HandleCrashedAircraft_v_0)
    GetSlopePixelZ();
  if (__trans_tmp_1)
    st->airport.flags &= RUNWAY_IN_block;
}
