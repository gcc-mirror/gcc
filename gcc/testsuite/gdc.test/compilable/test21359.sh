#!/usr/bin/env bash

objfile="${OUTPUT_BASE}${OBJ}"
$DMD -c -m${MODEL} -allinst -of${objfile} ${EXTRA_FILES}/${TEST_NAME}a.d -I${EXTRA_FILES}

# This test is brittle and might break in the future.
# For now, ensure a specific AA TypeInfo isn't defined multiple times, as happened with DMD v2.111.0.
mangled_sym='_D14TypeInfo_HiAya6__initZ'
num_typeinfo_syms=$(nm --defined-only ${objfile} | grep -F ${mangled_sym} | wc -l)
if [[ "${num_typeinfo_syms}" -ne 1 ]]; then
    echo "Expected 1 '${mangled_sym}' symbol definition but got: ${num_typeinfo_syms}"
    exit 1
fi

rm_retry ${objfile}
