/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mabi=64" { target { mips*-*-linux* && mips64 } } } */

struct offset_v1 {
    int k_uniqueness;
};

struct offset_v2 {
 long v;
} __attribute__ ((__packed__));

struct reiserfs_key {
    int k_objectid;
    union {
 struct offset_v1 k_offset_v1;
 struct offset_v2 k_offset_v2;
    } u;
};

struct item_head
{
 struct reiserfs_key ih_key;
 int ih_version;
};

static void set_offset_v2_k_type(struct offset_v2 *v2)
{
    v2->v &= 1;
}

static void set_le_key_k_type (int version, struct reiserfs_key * key)
{
    version ? (key->u.k_offset_v1.k_uniqueness = 1)
	    : set_offset_v2_k_type(&(key->u.k_offset_v2));
}

static void set_le_ih_k_type (struct item_head * ih)
{
    set_le_key_k_type((__builtin_constant_p((ih)->ih_version) ? (ih)->ih_version : (ih)->ih_version), &(ih->ih_key));
}

void boo(struct item_head *ih, const char *body);

void direct2indirect(void)
{
    struct item_head *p_le_ih;
    struct item_head ind_ih;
    unsigned int unfm_ptr;

    if (__builtin_expect(32, 0)) __asm__ ("break");

    set_le_ih_k_type (&ind_ih);

    if (__builtin_constant_p(p_le_ih) ? 1 : 2) {
        (__builtin_constant_p(__builtin_constant_p(1) == 1));
      boo(&ind_ih, (char *)&unfm_ptr);
    }
}
