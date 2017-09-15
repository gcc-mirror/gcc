/* Test support of scalar_storage_order pragma */

/* { dg-do compile } */
/* { dg-options "-Wall" } */

#pragma scalar_storage_order little-endian /* { dg-warning "not supported" } */
