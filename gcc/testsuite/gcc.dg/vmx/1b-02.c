/* { dg-do compile } */
#include <altivec.h>
vector unsigned char u8;
vector signed char s8;
vector bool char b8;
vector unsigned short u16;
vector signed short s16;
vector bool short b16;
vector unsigned int u32;
vector signed int s32;
vector bool int b32;
vector float f32;
vector pixel p16;

vector unsigned char const u8c;
vector signed char const s8c;
vector bool char const b8c;
vector unsigned short const u16c;
vector signed short const s16c;
vector bool short const b16c;
vector unsigned int const u32c;
vector signed int const s32c;
vector bool int const b32c;
vector float const f32c;
vector pixel const p16c;

vector unsigned char volatile u8v;
vector signed char volatile s8v;
vector bool char volatile b8v;
vector unsigned short volatile u16v;
vector signed short volatile s16v;
vector bool short volatile b16v;
vector unsigned int volatile u32v;
vector signed int volatile s32v;
vector bool int volatile b32v;
vector float volatile f32v;
vector pixel volatile p16v;

const vector unsigned char u8c_;
const vector signed char s8c_;
const vector bool char b8c_;
const vector unsigned short u16c_;
const vector signed short s16c_;
const vector bool short b16c_;
const vector unsigned int u32c_;
const vector signed int s32c_;
const vector bool int b32c_;
const vector float f32c_;
const vector pixel p16c_;

volatile vector unsigned char u8v_;
volatile vector signed char s8v_;
volatile vector bool char b8v_;
volatile vector unsigned short u16v_;
volatile vector signed short s16v_;
volatile vector bool short b16v_;
volatile vector unsigned int u32v_;
volatile vector signed int s32v_;
volatile vector bool int b32v_;
volatile vector float f32v_;
volatile vector pixel p16v_;
