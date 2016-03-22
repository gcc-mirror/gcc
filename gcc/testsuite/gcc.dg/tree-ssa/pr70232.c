/* { dg-do compile } */
/* { dg-options "-O2 -w -fdump-tree-vrp1-details -fdump-tree-vrp2-details -fdump-tree-dom2-details -fdump-tree-dom3-details" } */

/* All the threads found by the FSM threader should have too
   many statements to be profitable.  */
/* { dg-final { scan-tree-dump-not "Registering FSM " "dom2"} } */
/* { dg-final { scan-tree-dump-not "Registering FSM " "dom3"} } */
/* { dg-final { scan-tree-dump-not "Registering FSM " "vrp1"} } */
/* { dg-final { scan-tree-dump-not "Registering FSM " "vrp2"} } */

typedef _Bool bool;
typedef unsigned char uint8_t;
typedef unsigned long uint32_t;
typedef unsigned long long uint64_t;
typedef unsigned int size_t;

enum {
 false = 0,
 true = 1
};

struct list_head {
 struct list_head *next, *prev;
};


extern void * memcpy(void *, const void *, size_t);
extern int memcmp(const void *,const void *,size_t);
extern void * memset(void *, int, size_t);
extern void __memzero(void *ptr, size_t n);

static inline uint64_t wwn_to_uint64_t(uint8_t *wwn)
{
 return (uint64_t)wwn[0] << 56 | (uint64_t)wwn[1] << 48 |
     (uint64_t)wwn[2] << 40 | (uint64_t)wwn[3] << 32 |
     (uint64_t)wwn[4] << 24 | (uint64_t)wwn[5] << 16 |
     (uint64_t)wwn[6] << 8 | (uint64_t)wwn[7];
}

struct lpfc_name {
 union {
  uint8_t wwn[8];
 } u;
};

struct lpfc_hba {
 uint32_t cfg_fof;
 uint32_t cfg_oas_flags;
 struct list_head luns;
};

struct lpfc_device_id {
 struct lpfc_name vport_wwpn;
 struct lpfc_name target_wwpn;
 uint64_t lun;
};

struct lpfc_device_data {
 struct list_head listentry;
 struct lpfc_device_id device_id;
 bool oas_enabled;
 bool available;
};

bool
lpfc_find_next_oas_lun(struct lpfc_hba *phba, struct lpfc_name *vport_wwpn,
         struct lpfc_name *target_wwpn, uint64_t *starting_lun,
         struct lpfc_name *found_vport_wwpn,
         struct lpfc_name *found_target_wwpn,
         uint64_t *found_lun,
         uint32_t *found_lun_status)
{

 struct lpfc_device_data *lun_info;
 struct lpfc_device_id *device_id;
 uint64_t lun;
 bool found = false;

 if (__builtin_expect(!!(!phba), 0) || !vport_wwpn || !target_wwpn ||
     !starting_lun || !found_vport_wwpn ||
     !found_target_wwpn || !found_lun || !found_lun_status ||
     (*starting_lun == -1u) ||
     !phba->cfg_fof)
  return false;

 lun = *starting_lun;
 *found_lun = -1;
 *starting_lun = -1;



 for (lun_info = ({ const typeof( ((typeof(*lun_info) *)0)->listentry ) *__mptr = ((&phba->luns)->next); (typeof(*lun_info) *)( (char *)__mptr - __builtin_offsetof(typeof(*lun_info), listentry) );}); &lun_info->listentry != (&phba->luns); lun_info = ({ const typeof( ((typeof(*(lun_info)) *)0)->listentry ) *__mptr = ((lun_info)->listentry.next); (typeof(*(lun_info)) *)( (char *)__mptr - __builtin_offsetof(typeof(*(lun_info)), listentry) );})) {
  if (((wwn_to_uint64_t(vport_wwpn->u.wwn) == 0) ||
       (memcmp(&lun_info->device_id.vport_wwpn, vport_wwpn,
       sizeof(struct lpfc_name)) == 0)) &&
      ((wwn_to_uint64_t(target_wwpn->u.wwn) == 0) ||
       (memcmp(&lun_info->device_id.target_wwpn, target_wwpn,
       sizeof(struct lpfc_name)) == 0)) &&
      (lun_info->oas_enabled)) {
   device_id = &lun_info->device_id;
   if ((!found) && ((lun == 0) || (device_id->lun == lun))) {
    *found_lun = device_id->lun;
    memcpy(found_vport_wwpn, &device_id->vport_wwpn, sizeof(struct lpfc_name));
    memcpy(found_target_wwpn, &device_id->target_wwpn, sizeof(struct lpfc_name));
    if (lun_info->available)
     *found_lun_status = 0x01;
    else
     *found_lun_status = 0;
    if (phba->cfg_oas_flags & 0x01)
     ({ void *__p = (vport_wwpn); size_t __n = sizeof(struct lpfc_name); if ((__n) != 0) { if (__builtin_constant_p((0x0)) && (0x0) == 0) __memzero((__p),(__n)); else memset((__p),(0x0),(__n)); } (__p); })
                                     ;
    if (phba->cfg_oas_flags & 0x02)
     ({ void *__p = (target_wwpn); size_t __n = sizeof(struct lpfc_name); if ((__n) != 0) { if (__builtin_constant_p((0x0)) && (0x0) == 0) __memzero((__p),(__n)); else memset((__p),(0x0),(__n)); } (__p); })
                                     ;
    found = true;
   } else if (found) {
    *starting_lun = device_id->lun;
    memcpy(vport_wwpn, &device_id->vport_wwpn,
           sizeof(struct lpfc_name));
    memcpy(target_wwpn, &device_id->target_wwpn,
           sizeof(struct lpfc_name));
    break;
   }
  }
 }
 return found;
}


