/* Minimized/hacked up from openvswitch lib/conntrack.c, which had this license
   header:  */
/*
 * Copyright (c) 2015-2019 Nicira, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)
#define false 0

#define OBJECT_OFFSETOF(OBJECT, MEMBER)\
    __builtin_offsetof(typeof(*(OBJECT)), MEMBER)

#define OBJECT_CONTAINING(POINTER, OBJECT, MEMBER)                      \
    ((typeof(OBJECT)) (void *)                                      \
     ((char *) (POINTER) - OBJECT_OFFSETOF(OBJECT, MEMBER)))

#define ASSIGN_CONTAINER(OBJECT, POINTER, MEMBER)			\
    ((OBJECT) = OBJECT_CONTAINING(POINTER, OBJECT, MEMBER), (void) 0)

#define INIT_CONTAINER(OBJECT, POINTER, MEMBER)				\
    ((OBJECT) = NULL, ASSIGN_CONTAINER(OBJECT, POINTER, MEMBER))

#define HMAP_FOR_EACH_POP(NODE, MEMBER, HMAP)				\
    for (size_t bucket__ = 0;                                               \
         INIT_CONTAINER(NODE, hmap_pop_helper__(HMAP, &bucket__), MEMBER),  \
         (NODE != OBJECT_CONTAINING(NULL, NODE, MEMBER))                    \
         || ((NODE = NULL), false);)

struct hmap {
    struct hmap_node **buckets;
    struct hmap_node *one;
    size_t mask;
    size_t n;
};

struct hmap_node {
    size_t hash;
    struct hmap_node *next;
};

static inline void hmap_remove(struct hmap *, struct hmap_node *);

struct hmap_node *
hmap_pop_helper__(struct hmap *hmap, size_t *bucket) {

    for (; *bucket <= hmap->mask; (*bucket)++) {
        struct hmap_node *node = hmap->buckets[*bucket];

        if (node) {
            hmap_remove(hmap, node);
            return node;
        }
    }

    return NULL;
}

static inline void
hmap_remove(struct hmap *hmap, struct hmap_node *node)
{
    struct hmap_node **bucket = &hmap->buckets[node->hash & hmap->mask];
    while (*bucket != node) {
        bucket = &(*bucket)->next;
    }
    *bucket = node->next;
    hmap->n--;
}

struct conntrack {
    struct hmap zone_limits;
};

struct zone_limit {
    struct hmap_node node;
};

void
conntrack_destroy(struct conntrack *ct)
{
    struct zone_limit *zl;
    HMAP_FOR_EACH_POP (zl, node, &ct->zone_limits) {
      __builtin_free(zl);
    }
}
