typedef struct ec_key_st EC_KEY;
typedef struct ec_group_st EC_GROUP;
typedef struct R3410_ec {
  int nid;
  EC_GROUP *group;
} R3410_ec_params;
extern R3410_ec_params R3410_2012_512_paramset[];

static R3410_ec_params *gost_nid2params(int nid) {
  R3410_ec_params *params;

  params = R3410_2012_512_paramset;
  while (params->nid != 0) {
    if (params->nid == nid)
      return params;
    params++;
  }

  return ((void *)0);
}

int fill_GOST_EC_params(EC_KEY *eckey, int nid) {
  R3410_ec_params *params = gost_nid2params(nid);
  if (!eckey || !params) {
    return 0;
  }

  if (params->group) {
    return 1;
  }

  return 0;
}
