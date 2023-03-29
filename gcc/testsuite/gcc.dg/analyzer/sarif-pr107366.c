/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-format=sarif-file" } */

typedef enum {
  HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO
} hwloc_topology_diff_obj_attr_type_t;
enum { HWLOC_TOPOLOGY_DIFF_OBJ_ATTR } hwloc_apply_diff_one_diff_0_0;

void
hwloc_apply_diff_one() {
  switch (hwloc_apply_diff_one_diff_0_0)
  case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR: {
    hwloc_topology_diff_obj_attr_type_t obj_attr_2_0_0;
    switch (obj_attr_2_0_0)
    case HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO: {
      unsigned ii = 0;
    }
  }
}

