/* Test the fix for PR92142. */

#include <ISO_Fortran_binding.h>

#include <stdlib.h>

int c_setpointer(CFI_cdesc_t *);

int c_setpointer(CFI_cdesc_t *ip)
{
  CFI_cdesc_t *yp = NULL;
  void *auxp = ip->base_addr;
  int ierr;
  int status;

  /* Setting up the pointer */
  ierr = 1;
  yp = malloc(sizeof(*ip));
  if (yp == NULL) return ierr;
  status = CFI_establish(yp, NULL, CFI_attribute_pointer, ip->type, ip->elem_len, ip->rank, NULL);
  if (status != CFI_SUCCESS) return ierr;
  if (yp->attribute != CFI_attribute_pointer) return ierr;
  /* Set the pointer to ip */
  ierr = 2;
  status = CFI_setpointer(yp, ip, NULL);
  if (status != CFI_SUCCESS) return ierr;
  if (yp->attribute != CFI_attribute_pointer) return ierr;
  /* Set the pointer to NULL */
  ierr = 3;
  status = CFI_setpointer(yp, NULL, NULL);
  if (status != CFI_SUCCESS) return ierr;
  if (yp->attribute != CFI_attribute_pointer) return ierr;
  /* "Set" the ip variable to yp (should not be possible) */
  ierr = 4;
  status = CFI_setpointer(ip, yp, NULL);
  if (status != CFI_INVALID_ATTRIBUTE) return ierr;
  if (ip->attribute != CFI_attribute_other) return ierr;
  if (ip->base_addr != auxp) return ierr;
  return 0;
}
