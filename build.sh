mkdir build &&
cd build &&
../configure --enable-languages=fortran \
--disable-multilib \
--disable-bootstrap &&
make -j 4
