/* { dg-do compile } */
/* { dg-options "-O" } */

typedef struct chunk_t chunk_t;
struct chunk_t
{
  unsigned char *ptr;
  long unsigned int len;
};
extern chunk_t asn1_wrap (chunk_t c, ...);
typedef struct linked_list_t linked_list_t;
chunk_t ietfAttr_list_encode (linked_list_t * list);
extern linked_list_t *groups;
static unsigned char ASN1_group_oid_str[] = {
    0x06
};
static const chunk_t ASN1_group_oid = {
  ASN1_group_oid_str, sizeof (ASN1_group_oid_str)
};
static chunk_t
build_attribute_type (const chunk_t type, chunk_t content)
{
  return type;
}
static chunk_t
build_attributes (void)
{
  return asn1_wrap (build_attribute_type (ASN1_group_oid,
					  ietfAttr_list_encode (groups)));
}
void build_attr_cert (void)
{
  asn1_wrap (build_attributes ());
}
