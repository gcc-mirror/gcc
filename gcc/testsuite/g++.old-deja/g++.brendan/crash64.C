// Build don't link: 
// GROUPS passed old-abort
typedef long unsigned int size_t;
typedef void (*RF_Ptr)(void *);

struct _im_pers_mem_spec {
  inline _im_pers_mem_spec(void );
  inline _im_pers_mem_spec(auto int of, auto int n);
};

struct _type_desc {
  _type_desc(char *, int , RF_Ptr , int , int ,...);
};

struct metatype { int base_list; };

static _type_desc _type_metatype("metatype", sizeof(metatype),
  (RF_Ptr)0, 0, 1, 1,
  _im_pers_mem_spec( ((size_t)&((( metatype *)0)-> base_list )) , 1)); // WARNING - cannot pass non-pod
