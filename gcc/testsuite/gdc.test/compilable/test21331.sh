#!/usr/bin/env bash

objfile="${OUTPUT_BASE}${OBJ}"
$DMD -c -m${MODEL} -of${objfile} ${EXTRA_FILES}/${TEST_NAME}.d

nm ${objfile} | grep -E '16__lambda_L13_C17.+15__lambda_L5_C19'
nm ${objfile} | grep -E '16__lambda_L13_C17.+17__lambda_L5_C19_1'
nm ${objfile} | grep -E '18__lambda_L13_C17_1.+15__lambda_L5_C19'
nm ${objfile} | grep -E '18__lambda_L13_C17_1.+17__lambda_L5_C19_1'

rm_retry ${objfile}
