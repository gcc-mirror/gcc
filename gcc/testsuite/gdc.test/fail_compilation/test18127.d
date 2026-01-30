/*
TEST_OUTPUT:
---
fail_compilation/imports/imp18127b.c(3): Error: struct `struct_or_union` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(3):        previously declared here
fail_compilation/imports/imp18127b.c(3):        `imp18127b.struct_or_union` is a struct while `imp18127a.struct_or_union` is a union
fail_compilation/imports/imp18127b.c(3):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(8): Error: struct `S_n_fields` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(8):        previously declared here
fail_compilation/imports/imp18127b.c(8):        `imp18127b.S_n_fields` has 1 field(s) while `imp18127a.S_n_fields` has 2 field(s)
fail_compilation/imports/imp18127b.c(8):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(13): Error: struct `S_types` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(13):        previously declared here
fail_compilation/imports/imp18127b.c(14):        Field 0 differs in type
fail_compilation/imports/imp18127b.c(14):        typeof(x): int
fail_compilation/imports/imp18127a.c(14):        typeof(x): float
fail_compilation/imports/imp18127b.c(13):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(18): Error: struct `S_names` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(18):        previously declared here
fail_compilation/imports/imp18127b.c(19):        Field 0 differs in name
fail_compilation/imports/imp18127b.c(19):        y
fail_compilation/imports/imp18127a.c(19):        x
fail_compilation/imports/imp18127b.c(18):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(22): Error: struct `B` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(22):        previously declared here
fail_compilation/imports/imp18127b.c(23):        Field 0 differs in type
fail_compilation/imports/imp18127b.c(23):        typeof(x): float
fail_compilation/imports/imp18127a.c(23):        typeof(x): int
fail_compilation/imports/imp18127b.c(22):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(27): Error: struct `S_b` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(27):        previously declared here
fail_compilation/imports/imp18127b.c(27):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(33): Error: struct `(anonymous struct)` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(33):        previously declared here
fail_compilation/imports/imp18127b.c(34):        Field 0 differs in type
fail_compilation/imports/imp18127b.c(34):        typeof(x): float
fail_compilation/imports/imp18127a.c(34):        typeof(x): int
fail_compilation/imports/imp18127b.c(32): Error: struct `S_contains_anon_named` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(32):        previously declared here
fail_compilation/imports/imp18127b.c(35):        Field 0 differs in type
fail_compilation/imports/imp18127b.c(35):        typeof(a): (anonymous struct)
fail_compilation/imports/imp18127a.c(35):        typeof(a): (anonymous struct)
fail_compilation/imports/imp18127b.c(32):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(38): Error: struct `S_contains_anon_unnamed` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(38):        previously declared here
fail_compilation/imports/imp18127b.c(40):        Field 0 differs in type
fail_compilation/imports/imp18127b.c(40):        typeof(x): float
fail_compilation/imports/imp18127a.c(40):        typeof(x): int
fail_compilation/imports/imp18127b.c(38):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(45): Error: struct `S_bitfields_mismatch1` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(45):        previously declared here
fail_compilation/imports/imp18127b.c(47):        Field 1 differs in being a bitfield
fail_compilation/imports/imp18127b.c(47):        `imp18127b.S_bitfields_mismatch1.y` is not a bitfield
fail_compilation/imports/imp18127a.c(47):        `imp18127a.S_bitfields_mismatch1.y` is a bitfield
fail_compilation/imports/imp18127b.c(45):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(49): Error: struct `S_bitfields_mismatch2` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(49):        previously declared here
fail_compilation/imports/imp18127b.c(50):        Field 0 differs in being a bitfield
fail_compilation/imports/imp18127b.c(50):        `imp18127b.S_bitfields_mismatch2.x` *is a bitfield
fail_compilation/imports/imp18127a.c(50):        `imp18127a.S_bitfields_mismatch2.x` is not a bitfield
fail_compilation/imports/imp18127b.c(49):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(54): Error: struct `S_bitfields_widths` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(54):        previously declared here
fail_compilation/imports/imp18127b.c(56):        Field 1 differs in bitfield width
fail_compilation/imports/imp18127b.c(56):        `imp18127b.S_bitfields_widths.y`: 2
fail_compilation/imports/imp18127a.c(56):        `imp18127a.S_bitfields_widths.y`: 1
fail_compilation/imports/imp18127b.c(54):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(59): Error: struct `S_bitfields_anon` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(59):        previously declared here
fail_compilation/imports/imp18127b.c(61):        Field 1 differs in name
fail_compilation/imports/imp18127b.c(61):        y
fail_compilation/imports/imp18127a.c(61):        (anonymous)
fail_compilation/imports/imp18127b.c(59):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(65): Error: struct `S_alignas` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(65):        previously declared here
fail_compilation/imports/imp18127b.c(66):        Field 0 differs in alignment or packing
fail_compilation/imports/imp18127b.c(66):        `imp18127b.S_alignas.x` alignment: default
fail_compilation/imports/imp18127a.c(66):        `imp18127a.S_alignas.x` alignment: 8
fail_compilation/imports/imp18127b.c(65):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(68): Error: struct `S_aligned` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(68):        previously declared here
fail_compilation/imports/imp18127b.c(68):        `imp18127b.S_aligned` has different alignment or packing
fail_compilation/imports/imp18127b.c(68):        `imp18127b.S_aligned` alignment: 4
fail_compilation/imports/imp18127a.c(68):        `imp18127a.S_aligned` alignment: 8
fail_compilation/imports/imp18127b.c(68):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(73): Error: struct `S_pack_1` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(73):        previously declared here
fail_compilation/imports/imp18127b.c(73):        `imp18127b.S_pack_1` has different alignment or packing
fail_compilation/imports/imp18127b.c(73):        `imp18127b.S_pack_1` alignment: default
fail_compilation/imports/imp18127a.c(73):        `imp18127a.S_pack_1` alignment: 1
fail_compilation/imports/imp18127b.c(73):        `imp18127b.S_pack_1` packed: false
fail_compilation/imports/imp18127a.c(73):        `imp18127a.S_pack_1` packed: true
fail_compilation/imports/imp18127b.c(73):        C structs with the same name from different imports are merged
fail_compilation/imports/imp18127b.c(77): Error: struct `S_pack_2` already exists with an incompatible definition.
fail_compilation/imports/imp18127a.c(79):        previously declared here
fail_compilation/imports/imp18127b.c(77):        `imp18127b.S_pack_2` has different alignment or packing
fail_compilation/imports/imp18127b.c(77):        `imp18127b.S_pack_2` alignment: default
fail_compilation/imports/imp18127a.c(79):        `imp18127a.S_pack_2` alignment: 1
fail_compilation/imports/imp18127b.c(77):        C structs with the same name from different imports are merged
---
*/

// https://github.com/dlang/dmd/issues/18127

import imports.imp18127a;
import imports.imp18127b;
