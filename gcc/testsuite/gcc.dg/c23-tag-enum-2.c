/*
 * { dg-do compile }
 * { dg-options "-std=c23" }
 */

enum A { B = 7 } y;

// complete during later construction

enum A { B = _Generic(&y, enum A*: 7, default: 1) };


enum X { E = 1, F = 1 + 1 };
enum X { F = 2, E = 1 };



