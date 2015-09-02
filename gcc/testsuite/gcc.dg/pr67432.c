/* PR c/67432 */
/* { dg-do compile } */

enum {}; /* { dg-error "empty enum is invalid" } */
enum E {}; /* { dg-error "empty enum is invalid" } */
enum F {} e; /* { dg-error "empty enum is invalid" } */
