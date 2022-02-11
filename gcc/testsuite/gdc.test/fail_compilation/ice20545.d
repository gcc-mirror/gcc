/*
TEST_OUTPUT:
---
fail_compilation/ice20545.d(8): Error: initializer expression expected following colon, not `]`
---
*/

static initial = [{ }: ];
