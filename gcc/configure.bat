@echo off
rem GCC configuration script for MSDOG

rem Configure for our environment
copy config\i386.xmh config.h
copy config\i386gas.tmh tm.h
copy config\i386.md md
copy config\out-i386.c aux-output.c

rem Install our makefile
copy Makefile.dos Makefile
