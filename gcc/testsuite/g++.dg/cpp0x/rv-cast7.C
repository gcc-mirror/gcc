// PR c++/93862 - ICE with static_cast when converting from int[].
// { dg-do compile { target c++11 } }

int(&&intu_rvref)[]{1,2,3,4};
int(&int4_lvref)[4] = static_cast<int(&)[4]>(intu_rvref); // { dg-error "invalid .static_cast." }
int(&&int4_rvref)[4] = static_cast<int(&&)[4]>(intu_rvref); // { dg-error "invalid .static_cast." }
