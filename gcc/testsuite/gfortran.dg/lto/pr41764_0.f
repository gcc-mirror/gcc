! { dg-lto-do link }
! FIXME: This test used to fail with gold and -fuse-linker-plugin. It is
! here for people testing with RUNTESTFLAGS=-fuse-linker-plugin, but it would
! be nice to create "dg-effective-target-supports linker-plugin" and use it.
      PROGRAM INIRAN
      INTEGER IX, IY, IZ
      COMMON /XXXRAN/ IX, IY, IZ
      END
      BLOCKDATA RAEWIN
      INTEGER IX, IY, IZ
      COMMON /XXXRAN/ IX, IY, IZ
      DATA IX, IY, IZ / 1974, 235, 337 /
      END
