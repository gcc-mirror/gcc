/* PR target/78057 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -mlzcnt -fdump-tree-optimized" } */

extern void link_error (void);

int
foo (int x, long long y)
{
  if (__builtin_ia32_tzcnt_u16 (16) != 4
      || __builtin_ia32_tzcnt_u16 (0) != 16
      || __builtin_ia32_lzcnt_u16 (0x1ff) != 7
      || __builtin_ia32_lzcnt_u16 (0) != 16
      || __builtin_ia32_tzcnt_u32 (8) != 3
      || __builtin_ia32_tzcnt_u32 (0) != 32
      || __builtin_ia32_lzcnt_u32 (0x3fffffff) != 2
      || __builtin_ia32_lzcnt_u32 (0) != 32
#ifdef __x86_64__
      || __builtin_ia32_tzcnt_u64 (4) != 2
      || __builtin_ia32_tzcnt_u64 (0) != 64
      || __builtin_ia32_lzcnt_u64 (0x1fffffff) != 35
      || __builtin_ia32_lzcnt_u64 (0) != 64
#endif
     )
    link_error ();
  x += 2;
  y += 2;
  if (x == 0 || y == 0)
    return 5;
  return __builtin_ia32_tzcnt_u32 (x)
         + __builtin_ia32_lzcnt_u32 (x)
#ifdef __x86_64__
	 + __builtin_ia32_tzcnt_u64 (y)
	 + __builtin_ia32_lzcnt_u64 (y)
#endif
	 ;
}

/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_ia32_\[lt]zcnt" "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ctz " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_clz " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ctzll " 1 "optimized" { target lp64 } } } */
