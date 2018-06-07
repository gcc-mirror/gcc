/* { dg-do compile } */
/* { dg-options "-Os -fno-strict-overflow" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef u16 acpi_rs_length;
typedef u32 acpi_rsdesc_size;
struct acpi_resource_source
{
  u16 string_length;
  char *string_ptr;
};
static u16
acpi_rs_strcpy (char *destination, char *source)
{
  u16 i;
  for (i = 0; source[i]; i++)
    {
    }
  return ((u16) (i + 1));
}
union aml_resource;
acpi_rs_length
acpi_rs_get_resource_source (acpi_rs_length resource_length,
			     acpi_rs_length minimum_length,
			     struct acpi_resource_source * resource_source,
			     union aml_resource * aml, char *string_ptr)
{
  acpi_rsdesc_size total_length;
  u8 *aml_resource_source;
  if (total_length > (acpi_rsdesc_size) (minimum_length + 1))
    {
      resource_source->string_length =
	acpi_rs_strcpy (resource_source->string_ptr,
			((char *) (void *) (&aml_resource_source[1])));
    }
}
